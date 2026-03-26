#!/bin/bash
# ============================================================
# 🧪 LQAS PIPELINE TEST SCRIPT
# Purpose: Validate full pipeline (Python + R + structure)
# ============================================================

set -e  # stop on error

# Colors for better output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}🧪 LQAS PIPELINE TEST${NC}"
echo -e "${BLUE}========================================${NC}"

# Track test results
TESTS_PASSED=0
TESTS_FAILED=0
WARNINGS=0

# Function to report test result
report_test() {
    local test_name="$1"
    local result="$2"
    local message="$3"
    
    if [ "$result" == "pass" ]; then
        echo -e "${GREEN}✅ ${test_name}: ${message}${NC}"
        ((TESTS_PASSED++))
    elif [ "$result" == "fail" ]; then
        echo -e "${RED}❌ ${test_name}: ${message}${NC}"
        ((TESTS_FAILED++))
    elif [ "$result" == "warn" ]; then
        echo -e "${YELLOW}⚠️  ${test_name}: ${message}${NC}"
        ((WARNINGS++))
    fi
}

# ------------------------------------------------------------
# 1. CHECK REQUIRED DIRECTORIES
# ------------------------------------------------------------
echo -e "\n${BLUE}📁 Step 1: Checking directory structure...${NC}"

REQUIRED_DIRS=(
    ".github/workflows"
    "config"
    "src/python"
    "src/r"
    "scripts"
    "tests"
    "data/input"
    "data/output"
    "data/archive"
    "data/metadata"
    "logs"
    "reports"
)

for dir in "${REQUIRED_DIRS[@]}"; do
    if [ -d "$dir" ]; then
        report_test "Directory: $dir" "pass" "exists"
    else
        report_test "Directory: $dir" "fail" "missing"
    fi
done

# ------------------------------------------------------------
# 2. CHECK REQUIRED FILES
# ------------------------------------------------------------
echo -e "\n${BLUE}📄 Step 2: Checking required files...${NC}"

REQUIRED_FILES=(
    ".github/workflows/lqas-pipeline.yml"
    "config/config.yml"
    "config/form_ids.json"
    "src/python/fetch_ona_data.py"
    "src/python/requirements.txt"
    "src/r/process_lqas.R"
    "scripts/manual_trigger.sh"
    "scripts/check_status.sh"
    "scripts/push_to_git.sh"
    "scripts/run_pipeline.sh"
    "tests/test_workflow.sh"
    "README.md"
)

for file in "${REQUIRED_FILES[@]}"; do
    if [ -f "$file" ]; then
        report_test "File: $file" "pass" "exists"
    else
        report_test "File: $file" "fail" "missing"
    fi
done

# ------------------------------------------------------------
# 3. CHECK FILE PERMISSIONS (Unix/Linux/Mac only)
# ------------------------------------------------------------
if [[ "$OSTYPE" != "msys" ]] && [[ "$OSTYPE" != "win32" ]] && [[ "$OSTYPE" != "cygwin" ]]; then
    echo -e "\n${BLUE}🔐 Step 3: Checking script permissions...${NC}"
    
    SCRIPT_FILES=(
        "scripts/manual_trigger.sh"
        "scripts/check_status.sh"
        "scripts/push_to_git.sh"
        "scripts/run_pipeline.sh"
        "tests/test_workflow.sh"
    )
    
    for script in "${SCRIPT_FILES[@]}"; do
        if [ -f "$script" ]; then
            if [ -x "$script" ]; then
                report_test "Permission: $script" "pass" "executable"
            else
                report_test "Permission: $script" "warn" "not executable"
            fi
        fi
    done
fi

# ------------------------------------------------------------
# 4. CHECK PYTHON ENVIRONMENT
# ------------------------------------------------------------
echo -e "\n${BLUE}🐍 Step 4: Checking Python environment...${NC}"

if command -v python &> /dev/null; then
    PYTHON_VERSION=$(python --version 2>&1)
    report_test "Python" "pass" "$PYTHON_VERSION"
    
    # Check for required Python packages
    if [ -f "src/python/requirements.txt" ]; then
        echo "   Installing/checking Python dependencies..."
        if pip install -r src/python/requirements.txt --quiet 2>/dev/null; then
            report_test "Python dependencies" "pass" "installed"
        else
            report_test "Python dependencies" "warn" "some packages may need manual installation"
        fi
    fi
else
    report_test "Python" "fail" "not installed"
fi

# ------------------------------------------------------------
# 5. CHECK R ENVIRONMENT
# ------------------------------------------------------------
echo -e "\n${BLUE}📊 Step 5: Checking R environment...${NC}"

if command -v Rscript &> /dev/null; then
    R_VERSION=$(Rscript --version 2>&1 | head -n1)
    report_test "R" "pass" "$R_VERSION"
    
    # Check for required R packages
    echo "   Checking R packages..."
    Rscript -e "
        required <- c('tidyverse', 'qs', 'logger', 'fs', 'argparse', 'janitor')
        missing <- required[!sapply(required, requireNamespace, quietly = TRUE)]
        if (length(missing) > 0) {
            cat('WARNING: Missing packages: ', paste(missing, collapse=', '), '\n')
            quit(status = 1)
        } else {
            cat('SUCCESS: All required packages installed\n')
            quit(status = 0)
        }
    " 2>/dev/null
    
    if [ $? -eq 0 ]; then
        report_test "R packages" "pass" "all required packages installed"
    else
        report_test "R packages" "warn" "some packages may need installation"
    fi
else
    report_test "R" "fail" "not installed"
fi

# ------------------------------------------------------------
# 6. VALIDATE CONFIGURATION FILES
# ------------------------------------------------------------
echo -e "\n${BLUE}⚙️  Step 6: Validating configuration files...${NC}"

# Check config.yml
if [ -f "config/config.yml" ]; then
    if grep -q "ona:" config/config.yml && grep -q "form_ids:" config/config.yml; then
        report_test "config.yml" "pass" "valid structure"
        
        # Count forms in config
        FORM_COUNT=$(grep -c "^ *- [0-9]" config/config.yml || echo "0")
        report_test "Forms in config" "pass" "$FORM_COUNT forms configured"
    else
        report_test "config.yml" "fail" "invalid structure"
    fi
fi

# Check form_ids.json
if [ -f "config/form_ids.json" ]; then
    if command -v python &> /dev/null; then
        if python -c "import json; json.load(open('config/form_ids.json'))" 2>/dev/null; then
            FORM_COUNT=$(python -c "import json; print(len(json.load(open('config/form_ids.json')).get('lqas_forms', [])))")
            report_test "form_ids.json" "pass" "valid JSON with $FORM_COUNT forms"
        else
            report_test "form_ids.json" "fail" "invalid JSON"
        fi
    else
        report_test "form_ids.json" "pass" "file exists (validation skipped)"
    fi
fi

# ------------------------------------------------------------
# 7. CHECK GITHUB WORKFLOW FILE
# ------------------------------------------------------------
echo -e "\n${BLUE}🔄 Step 7: Validating GitHub workflow...${NC}"

if [ -f ".github/workflows/lqas-pipeline.yml" ]; then
    # Check for required workflow sections
    if grep -q "name:" .github/workflows/lqas-pipeline.yml && \
       grep -q "on:" .github/workflows/lqas-pipeline.yml && \
       grep -q "jobs:" .github/workflows/lqas-pipeline.yml; then
        report_test "GitHub workflow" "pass" "valid structure"
        
        # Check for required jobs
        if grep -q "fetch-data:" .github/workflows/lqas-pipeline.yml; then
            report_test "Workflow job: fetch-data" "pass" "configured"
        else
            report_test "Workflow job: fetch-data" "warn" "may be missing"
        fi
        
        if grep -q "process-data:" .github/workflows/lqas-pipeline.yml; then
            report_test "Workflow job: process-data" "pass" "configured"
        else
            report_test "Workflow job: process-data" "warn" "may be missing"
        fi
    else
        report_test "GitHub workflow" "fail" "invalid structure"
    fi
fi

# ------------------------------------------------------------
# 8. CHECK GIT STATUS
# ------------------------------------------------------------
echo -e "\n${BLUE}📦 Step 8: Checking Git status...${NC}"

if [ -d ".git" ]; then
    report_test "Git repository" "pass" "initialized"
    
    # Check if remote is configured
    if git remote -v 2>/dev/null | grep -q origin; then
        report_test "Git remote" "pass" "configured"
        
        # Get GitHub username and repo
        GITHUB_USER=$(git remote -v | grep fetch | head -n1 | sed -E 's/.*[:/]([^/]+)\/.*/\1/')
        REPO_NAME=$(git remote -v | grep fetch | head -n1 | sed -E 's/.*[:/][^/]+\/([^.]+)(\.git)? .*/\1/')
        report_test "GitHub repo" "pass" "$GITHUB_USER/$REPO_NAME"
    else
        report_test "Git remote" "warn" "no remote configured"
    fi
else
    report_test "Git repository" "fail" "not initialized"
fi

# ------------------------------------------------------------
# 9. CHECK GITHUB CLI
# ------------------------------------------------------------
echo -e "\n${BLUE}🔧 Step 9: Checking GitHub CLI...${NC}"

if command -v gh &> /dev/null; then
    GH_VERSION=$(gh --version 2>&1 | head -n1)
    report_test "GitHub CLI" "pass" "$GH_VERSION"
    
    # Check authentication
    if gh auth status &> /dev/null; then
        GH_USER=$(gh api user --jq '.login' 2>/dev/null)
        report_test "GitHub auth" "pass" "logged in as $GH_USER"
    else
        report_test "GitHub auth" "warn" "not authenticated (run 'gh auth login')"
    fi
else
    report_test "GitHub CLI" "warn" "not installed (optional)"
fi

# ------------------------------------------------------------
# 10. CREATE .gitkeep FILES IF MISSING
# ------------------------------------------------------------
echo -e "\n${BLUE}📁 Step 10: Ensuring data directories have .gitkeep...${NC}"

for dir in data/input data/output data/archive data/metadata logs reports; do
    if [ -d "$dir" ]; then
        if [ ! -f "$dir/.gitkeep" ] && [ ! -f "$dir/.gitignore" ]; then
            touch "$dir/.gitkeep"
            report_test ".gitkeep for $dir" "pass" "created"
        else
            report_test ".gitkeep for $dir" "pass" "exists"
        fi
    fi
done

# ------------------------------------------------------------
# 11. TEST PYTHON FETCH (OPTIONAL)
# ------------------------------------------------------------
echo -e "\n${BLUE}🚀 Step 11: Testing Python fetch (dry run)...${NC}"

if command -v python &> /dev/null; then
    # Test if script can be executed (without actually fetching)
    if python -c "import sys; sys.path.insert(0, 'src/python'); import fetch_ona_data" 2>/dev/null; then
        report_test "Python fetch script" "pass" "syntax valid"
    else
        report_test "Python fetch script" "fail" "syntax error"
    fi
else
    report_test "Python fetch test" "skip" "Python not available"
fi

# ------------------------------------------------------------
# 12. TEST R PROCESSING (OPTIONAL)
# ------------------------------------------------------------
echo -e "\n${BLUE}🔄 Step 12: Testing R processing (dry run)...${NC}"

if command -v Rscript &> /dev/null; then
    # Test if script can be loaded (without actually processing)
    if Rscript -e "source('src/r/process_lqas.R')" 2>/dev/null; then
        report_test "R processing script" "pass" "syntax valid"
    else
        report_test "R processing script" "warn" "may have syntax issues"
    fi
else
    report_test "R processing test" "skip" "R not available"
fi

# ------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------
echo -e "\n${BLUE}========================================${NC}"
echo -e "${BLUE}📊 TEST SUMMARY${NC}"
echo -e "${BLUE}========================================${NC}"
echo -e "${GREEN}✅ Passed: $TESTS_PASSED${NC}"
echo -e "${RED}❌ Failed: $TESTS_FAILED${NC}"
echo -e "${YELLOW}⚠️  Warnings: $WARNINGS${NC}"
echo -e "${BLUE}========================================${NC}"

# Final verdict
if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "\n${GREEN}🎉 ALL CRITICAL TESTS PASSED!${NC}"
    echo -e "${GREEN}✅ Your LQAS pipeline is ready to deploy!${NC}"
    echo -e "\n${BLUE}Next steps:${NC}"
    echo "  1. Set up GitHub secrets: gh secret set ONA_API_TOKEN --body \"your_token\""
    echo "  2. Push to GitHub: git push origin main"
    echo "  3. Trigger workflow: ./scripts/manual_trigger.sh"
    exit 0
else
    echo -e "\n${RED}❌ Some tests failed. Please fix issues before deploying.${NC}"
    echo -e "${YELLOW}Common fixes:${NC}"
    echo "  - Create missing directories: mkdir -p <directory>"
    echo "  - Install missing files from templates"
    echo "  - Run: chmod +x scripts/*.sh"
    exit 1
fi