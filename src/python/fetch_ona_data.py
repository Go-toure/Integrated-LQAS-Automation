#!/usr/bin/env python3
"""
LQAS Data Fetcher from ONA Platform
GitHub Actions Integrated Version
"""

import os
import sys
import json
import yaml
import logging
import hashlib
import requests
import pandas as pd
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Any
import argparse

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(sys.stdout),
        logging.FileHandler('logs/fetch.log')
    ]
)
logger = logging.getLogger(__name__)

class LQASDataFetcher:
    """Fetches LQAS data from ONA platform"""
    
    def __init__(self, config_path: str):
        self.config = self._load_config(config_path)
        self.api_token = os.environ.get('ONA_API_TOKEN')
        
        if not self.api_token:
            logger.error("❌ ONA_API_TOKEN not found in environment variables")
            sys.exit(1)
        
        self.session = requests.Session()
        self.session.headers.update({
            "Authorization": f"Token {self.api_token}",
            "User-Agent": "LQAS-Automation/1.0"
        })
        
        # Setup directories
        self.input_dir = Path('data/input')
        self.metadata_dir = Path('data/metadata')
        self.input_dir.mkdir(parents=True, exist_ok=True)
        self.metadata_dir.mkdir(parents=True, exist_ok=True)
        
        logger.info(f"✅ Fetcher initialized for {len(self.config['ona']['form_ids'])} forms")
    
    def _load_config(self, config_path: str) -> Dict:
        """Load configuration from YAML file"""
        try:
            with open(config_path, 'r') as f:
                config = yaml.safe_load(f)
            logger.info(f"✅ Config loaded from {config_path}")
            return config
        except Exception as e:
            logger.error(f"❌ Failed to load config: {e}")
            sys.exit(1)
    
    def _flatten_dict(self, data: Dict, parent_key: str = "", sep: str = "/") -> Dict:
        """Flatten nested dictionary for ONA data structure"""
        flattened = {}
        
        for key, value in data.items():
            if parent_key and not parent_key.endswith("Count_HH"):
                new_key = f"{parent_key}{sep}{key}"
            else:
                new_key = f"{parent_key}{key}" if parent_key else key
            
            if isinstance(value, dict):
                flattened.update(self._flatten_dict(value, new_key, sep=sep))
            elif isinstance(value, list):
                for i, item in enumerate(value, 1):
                    if isinstance(item, dict):
                        flattened.update(self._flatten_dict(item, f"{new_key}[{i}]", sep=sep))
                    else:
                        flattened[f"{new_key}[{i}]"] = item
            else:
                flattened[new_key] = value
        
        return flattened
    
    def _calculate_hash(self, file_path: Path) -> str:
        """Calculate MD5 hash of file"""
        if not file_path.exists():
            return ""
        with open(file_path, 'rb') as f:
            return hashlib.md5(f.read()).hexdigest()
    
    def _should_fetch(self, form_id: int) -> tuple:
        """Check if form needs to be fetched"""
        metadata_path = self.metadata_dir / f"{form_id}.json"
        
        if not metadata_path.exists():
            return True, "no_metadata"
        
        try:
            with open(metadata_path, 'r') as f:
                metadata = json.load(f)
            
            # Check if we fetched today
            last_fetch = datetime.fromisoformat(metadata['last_fetch'])
            if last_fetch.date() == datetime.now().date():
                return False, "already_today"
            
            # Check if it's been more than 24 hours
            hours_since = (datetime.now() - last_fetch).total_seconds() / 3600
            if hours_since < 24:
                return False, f"recent_{hours_since:.1f}h"
            
            return True, "needs_update"
            
        except Exception as e:
            logger.warning(f"Error reading metadata for {form_id}: {e}")
            return True, "metadata_error"
    
    def fetch_form(self, form_id: int) -> List[Dict]:
        """Fetch all data for a specific form"""
        all_data = []
        page = 1
        page_size = 10000
        
        logger.info(f"📡 Fetching form {form_id}...")
        
        while True:
            try:
                response = self.session.get(
                    f"{self.config['ona']['base_url']}/{form_id}.json",
                    params={"page": page, "page_size": page_size},
                    timeout=60
                )
                
                if response.status_code == 200:
                    data = response.json()
                    if not data:
                        break
                    
                    logger.info(f"  Page {page}: {len(data)} records")
                    all_data.extend(data)
                    page += 1
                    
                elif response.status_code == 401:
                    logger.error(f"❌ Authentication failed for form {form_id}")
                    break
                elif response.status_code == 404:
                    logger.warning(f"⚠️ Form {form_id} not found")
                    break
                else:
                    logger.error(f"❌ Error {response.status_code} for form {form_id}")
                    break
                    
            except requests.exceptions.Timeout:
                logger.error(f"❌ Timeout fetching form {form_id}")
                break
            except Exception as e:
                logger.error(f"❌ Exception fetching form {form_id}: {e}")
                break
        
        logger.info(f"✅ Form {form_id}: {len(all_data)} total records")
        return all_data
    
    def save_to_rds(self, data: List[Dict], form_id: int) -> bool:
        """Save data to RDS file using qs format"""
        if not data:
            logger.warning(f"⚠️ No data to save for form {form_id}")
            return False
        
        try:
            # Convert to DataFrame
            df = pd.DataFrame(data).fillna("").astype(str)
            
            # Extract GPS components if present
            if "GPS_hh" in df.columns:
                gps_parts = df["GPS_hh"].str.split(" ", expand=True)
                if len(gps_parts.columns) >= 3:
                    df["_GPS_hh_latitude"] = gps_parts[0]
                    df["_GPS_hh_longitude"] = gps_parts[1]
                    df["_GPS_hh_altitude"] = gps_parts[2]
            
            # Save path
            output_path = self.input_dir / f"{form_id}.rds"
            
            # Save using R's qs package
            import rpy2.robjects as robjects
            from rpy2.robjects import pandas2ri
            from rpy2.robjects.conversion import localconverter
            
            with localconverter(robjects.default_converter + pandas2ri.converter):
                r_df = robjects.conversion.py2rpy(df)
                
                # Ensure qs is installed
                robjects.r('''
                if (!requireNamespace("qs", quietly = TRUE)) {
                    install.packages("qs", repos = "https://cloud.r-project.org/")
                }
                ''')
                
                # Save with qs
                robjects.r(f"qs::qsave({r_df.r_repr()}, '{output_path.as_posix()}')")
            
            # Save metadata
            metadata = {
                'form_id': form_id,
                'records': len(df),
                'columns': len(df.columns),
                'file_size': output_path.stat().st_size,
                'file_hash': self._calculate_hash(output_path),
                'last_fetch': datetime.now().isoformat(),
                'github_run_id': os.environ.get('GITHUB_RUN_ID', 'local'),
                'github_sha': os.environ.get('GITHUB_SHA', 'local')
            }
            
            metadata_path = self.metadata_dir / f"{form_id}.json"
            with open(metadata_path, 'w') as f:
                json.dump(metadata, f, indent=2)
            
            logger.info(f"✅ Saved {len(df)} records to {output_path}")
            return True
            
        except Exception as e:
            logger.error(f"❌ Failed to save form {form_id}: {e}")
            return False
    
    def run(self, force_full: bool = False, specific_forms: List[int] = None) -> Dict:
        """Main execution method"""
        form_ids = specific_forms or self.config['ona']['form_ids']
        results = {}
        
        logger.info(f"🚀 Starting fetch for {len(form_ids)} forms")
        logger.info(f"Force full: {force_full}")
        
        for form_id in form_ids:
            # Check if we need to fetch
            if not force_full:
                should_fetch, reason = self._should_fetch(form_id)
                if not should_fetch:
                    logger.info(f"⏭️ Skipping form {form_id}: {reason}")
                    results[form_id] = {'status': 'skipped', 'reason': reason}
                    continue
            
            # Fetch data
            data = self.fetch_form(form_id)
            if data:
                success = self.save_to_rds(data, form_id)
                results[form_id] = {
                    'status': 'success' if success else 'failed',
                    'records': len(data)
                }
            else:
                results[form_id] = {'status': 'failed', 'records': 0}
        
        # Save summary
        summary = {
            'timestamp': datetime.now().isoformat(),
            'force_full': force_full,
            'results': results,
            'github_run_id': os.environ.get('GITHUB_RUN_ID'),
            'github_workflow': os.environ.get('GITHUB_WORKFLOW')
        }
        
        summary_path = Path('logs/fetch_summary.json')
        summary_path.parent.mkdir(parents=True, exist_ok=True)
        with open(summary_path, 'w') as f:
            json.dump(summary, f, indent=2)
        
        # Count successes
        success_count = sum(1 for r in results.values() if r['status'] == 'success')
        logger.info(f"✅ Fetch complete: {success_count}/{len(form_ids)} forms successful")
        
        return results

def main():
    parser = argparse.ArgumentParser(description='Fetch LQAS data from ONA')
    parser.add_argument('--config', default='config/config.yml', help='Config file path')
    parser.add_argument('--force-full', action='store_true', help='Force full fetch')
    parser.add_argument('--form-ids', help='Comma-separated form IDs to fetch')
    args = parser.parse_args()
    
    specific_forms = None
    if args.form_ids:
        specific_forms = [int(f.strip()) for f in args.form_ids.split(',')]
    
    fetcher = LQASDataFetcher(args.config)
    results = fetcher.run(force_full=args.force_full, specific_forms=specific_forms)
    
    # Exit with error if any failures
    failed = sum(1 for r in results.values() if r['status'] == 'failed')
    sys.exit(1 if failed > 0 else 0)

if __name__ == "__main__":
    main()