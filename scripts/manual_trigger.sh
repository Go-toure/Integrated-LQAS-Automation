#!/bin/bash
# ============================================================
# LQAS Pipeline Manual Trigger Script
# This script manually triggers the GitHub Actions workflow
# and monitors its execution
# ============================================================

set -e  # Exit on error

# Colors for better output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Get GitHub username from remote URL
get_github_username() {
    git remote -v 2>/dev/null | grep fetch | head -n1 | sed -E 's/.*[:/]([^/]+)\/.*/\1/'
}

# Get repository name
get_repo_name() {
    git remote -v 2>/dev/null | grep fetch | head -n1 | sed -E 's/.*[:/][^/]+\/([^.]+)(\.git)? .*/\1/'
}

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}🚀 LQAS Pipeline Manual Trigger${NC}"
echo -e "${BLUE}========================================${NC}"

# Check if gh CLI is installed
if ! command -v gh &> /dev/null; then
    echo -e "${RED}❌ GitHub CLI (gh) is not installed${NC}"
    echo -e "${YELLOW}Please install it first:${NC}"
    echo "  - Windows: winget install --id GitHub.cli"
    echo "  - Mac: brew install gh"
    echo "  - Linux: https://cli.github.com/manual/installation"
    exit 1
fi

# Check if logged in to GitHub CLI
if ! gh auth status &> /dev/null; then
    echo -e "${YELLOW}⚠️  Not logged in to GitHub CLI${NC}"
    echo -e "${BLUE}Please run: gh auth login${NC}"
    exit 1
fi

# Get GitHub username and repo
GITHUB_USERNAME=$(get_github_username)
REPO_NAME=$(get_repo_name)

if [ -z "$GITHUB_USERNAME" ] || [ -z "$REPO_NAME" ]; then
    echo -e "${RED}❌ Could not determine GitHub repository information${NC}"
    echo -e "${YELLOW}Make sure you're in a git repository with a remote origin${NC}"
    exit 1
fi

echo -e "${GREEN}✅ Repository: ${GITHUB_USERNAME}/${REPO_NAME}${NC}"

# Parse command line arguments
FORCE_FULL=false
SPECIFIC_FORMS=""
ENVIRONMENT="production"

while [[ $# -gt 0 ]]; do
    case $1 in
        --force-full)
            FORCE_FULL=true
            shift
            ;;
        --forms)
            SPECIFIC_FORMS="$2"
            shift 2
            ;;
        --env)
            ENVIRONMENT="$2"
            shift 2
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo "Options:"
            echo "  --force-full    Force full reprocessing of all files"
            echo "  --forms IDS     Comma-separated list of form IDs to process"
            echo "  --env ENV       Environment (production/staging/development)"
            echo "  --help          Show this help message"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Build workflow dispatch command
WORKFLOW_CMD="gh workflow run lqas-pipeline.yml"

if [ "$FORCE_FULL" = true ]; then
    WORKFLOW_CMD="$WORKFLOW_CMD -f force_full=true"
    echo -e "${YELLOW}🔄 Force full reprocessing enabled${NC}"
fi

if [ -n "$SPECIFIC_FORMS" ]; then
    WORKFLOW_CMD="$WORKFLOW_CMD -f specific_forms=\"$SPECIFIC_FORMS\""
    echo -e "${YELLOW}🎯 Processing specific forms: $SPECIFIC_FORMS${NC}"
fi

if [ -n "$ENVIRONMENT" ]; then
    WORKFLOW_CMD="$WORKFLOW_CMD -f environment=\"$ENVIRONMENT\""
    echo -e "${YELLOW}🌍 Environment: $ENVIRONMENT${NC}"
fi

# Confirm before running
echo ""
echo -e "${BLUE}This will trigger the LQAS pipeline with:${NC}"
echo -e "  Force Full: ${YELLOW}$FORCE_FULL${NC}"
echo -e "  Forms: ${YELLOW}${SPECIFIC_FORMS:-All forms}${NC}"
echo -e "  Environment: ${YELLOW}$ENVIRONMENT${NC}"
echo ""
read -p "Continue? (y/n) " -n 1 -r
echo ""
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo -e "${RED}Cancelled${NC}"
    exit 0
fi

# Trigger the workflow
echo -e "${BLUE}⏳ Triggering workflow...${NC}"
if eval $WORKFLOW_CMD; then
    echo -e "${GREEN}✅ Workflow triggered successfully${NC}"
else
    echo -e "${RED}❌ Failed to trigger workflow${NC}"
    exit 1
fi

# Get the latest run ID
sleep 2
RUN_ID=$(gh run list --workflow=lqas-pipeline.yml --limit 1 --json databaseId --jq '.[0].databaseId')

if [ -z "$RUN_ID" ]; then
    echo -e "${YELLOW}⚠️  Could not get run ID, but workflow was triggered${NC}"
    echo -e "${BLUE}Check runs at: https://github.com/${GITHUB_USERNAME}/${REPO_NAME}/actions${NC}"
else
    echo -e "${GREEN}📊 Run ID: ${RUN_ID}${NC}"
    
    # Ask if user wants to watch the run
    echo ""
    read -p "Watch the run in real-time? (y/n) " -n 1 -r
    echo ""
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo -e "${BLUE}Watching run #${RUN_ID}...${NC}"
        echo -e "${YELLOW}Press Ctrl+C to stop watching (run continues in background)${NC}"
        echo ""
        gh run watch $RUN_ID
    else
        echo -e "${BLUE}View run at: https://github.com/${GITHUB_USERNAME}/${REPO_NAME}/actions/runs/${RUN_ID}${NC}"
    fi
fi

# Show dashboard URL
echo ""
echo -e "${BLUE}========================================${NC}"
echo -e "${GREEN}📊 Dashboard URL:${NC}"
echo -e "https://${GITHUB_USERNAME}.github.io/${REPO_NAME}/latest/dashboard.html"
echo -e "${BLUE}========================================${NC}"
echo ""
echo -e "${YELLOW}Note: Dashboard may take 5-10 minutes to update after run completes${NC}"