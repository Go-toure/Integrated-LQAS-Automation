#!/bin/bash
# ============================================================
# Push to Git Script
# Commits and pushes changes to GitHub
# ============================================================

set -e

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}📤 Push to GitHub${NC}"
echo -e "${BLUE}========================================${NC}"

# Check for changes
if git diff --quiet && git diff --cached --quiet; then
    echo -e "${YELLOW}No changes to commit${NC}"
    exit 0
fi

# Show what will be committed
echo -e "${BLUE}Files to commit:${NC}"
git status --short

# Ask for commit message
echo ""
read -p "Commit message: " commit_message

if [ -z "$commit_message" ]; then
    commit_message="Update LQAS pipeline - $(date '+%Y-%m-%d %H:%M:%S')"
    echo -e "${YELLOW}Using default message: $commit_message${NC}"
fi

# Add and commit
git add .
git commit -m "$commit_message"

# Push
echo -e "${BLUE}Pushing to GitHub...${NC}"
git push origin main || git push origin master

echo -e "${GREEN}✅ Push complete!${NC}"
echo -e "${BLUE}Workflow will trigger automatically (if changes affect src/ or config/)${NC}"