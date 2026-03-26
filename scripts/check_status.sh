#!/bin/bash
# ============================================================
# Check Pipeline Status Script
# Shows the status of the latest workflow runs
# ============================================================

set -e

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}📊 LQAS Pipeline Status${NC}"
echo -e "${BLUE}========================================${NC}"

# Get GitHub username
GITHUB_USERNAME=$(git remote -v 2>/dev/null | grep fetch | head -n1 | sed -E 's/.*[:/]([^/]+)\/.*/\1/')
REPO_NAME=$(git remote -v 2>/dev/null | grep fetch | head -n1 | sed -E 's/.*[:/][^/]+\/([^.]+)(\.git)? .*/\1/')

# Show recent runs
echo -e "${BLUE}Recent workflow runs:${NC}"
gh run list --workflow=lqas-pipeline.yml --limit 5

# Show latest run details
LATEST_RUN=$(gh run list --workflow=lqas-pipeline.yml --limit 1 --json status,conclusion,startedAt,databaseId --jq '.[0]')
RUN_ID=$(echo $LATEST_RUN | jq -r '.databaseId')
STATUS=$(echo $LATEST_RUN | jq -r '.status')
CONCLUSION=$(echo $LATEST_RUN | jq -r '.conclusion')
STARTED=$(echo $LATEST_RUN | jq -r '.startedAt')

echo ""
echo -e "${BLUE}Latest run (#${RUN_ID}):${NC}"
echo -e "  Started: ${YELLOW}${STARTED}${NC}"
echo -e "  Status: ${YELLOW}${STATUS}${NC}"
echo -e "  Conclusion: ${YELLOW}${CONCLUSION:-Pending}${NC}"

# Show dashboard URL
echo ""
echo -e "${BLUE}Dashboard URL:${NC}"
echo -e "${GREEN}https://${GITHUB_USERNAME}.github.io/${REPO_NAME}/latest/dashboard.html${NC}"