# Copying Documentation to GitHub Wiki

This document explains how to copy the wiki documentation from this repository to your GitHub wiki.

## Understanding GitHub Wikis

GitHub wikis are **separate git repositories** located at:
```
https://github.com/owner/repo.wiki.git
```

The wiki repository is independent of the main repository, so you need to clone and push to it separately.

## Method 1: Manual Copy (Recommended for First Time)

### Step 1: Enable Wiki

1. Go to your repository on GitHub
2. Click **Settings**
3. Scroll to **Features** section
4. Check ☑️ **Wikis**

### Step 2: Initialize Wiki

1. Click the **Wiki** tab in your repository
2. Click **Create the first page**
3. Title: `Home`
4. Add some placeholder content
5. Click **Save Page**

### Step 3: Clone Wiki Repository

```bash
# Clone the wiki (note the .wiki.git suffix)
git clone https://github.com/cxd/scala-au.id.cxd.math.wiki.git

cd scala-au.id.cxd.math.wiki
```

### Step 4: Copy Documentation Files

```bash
# From the main repository root, copy all wiki-docs
cp ../scala-au.id.cxd.math/wiki-docs/*.md ./

# List files to verify
ls -la
```

### Step 5: Commit and Push

```bash
# Add all markdown files
git add *.md

# Commit
git commit -m "Add comprehensive documentation"

# Push to wiki
git push origin master
```

### Step 6: View Your Wiki

Visit: `https://github.com/cxd/scala-au.id.cxd.math/wiki`

## Method 2: Script Automation

Create a script to automate the process:

```bash
#!/bin/bash
# sync-wiki.sh - Sync documentation to GitHub wiki

WIKI_DIR="wiki-temp"
WIKI_URL="https://github.com/cxd/scala-au.id.cxd.math.wiki.git"

# Clone wiki if not exists
if [ ! -d "$WIKI_DIR" ]; then
    git clone $WIKI_URL $WIKI_DIR
fi

# Enter wiki directory
cd $WIKI_DIR

# Pull latest changes
git pull origin master

# Copy all markdown files from wiki-docs
cp ../wiki-docs/*.md ./

# Add changes
git add *.md

# Commit with timestamp
git commit -m "Update documentation - $(date +%Y-%m-%d)"

# Push to wiki
git push origin master

cd ..

echo "Wiki updated successfully!"
```

Make it executable:
```bash
chmod +x sync-wiki.sh
./sync-wiki.sh
```

## Method 3: GitHub Actions (Automated)

Create `.github/workflows/sync-wiki.yml`:

```yaml
name: Sync Wiki

on:
  push:
    branches:
      - main
    paths:
      - 'wiki-docs/**'

jobs:
  sync:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout main repo
        uses: actions/checkout@v2
        
      - name: Checkout wiki
        uses: actions/checkout@v2
        with:
          repository: ${{ github.repository }}.wiki
          path: wiki
          
      - name: Copy documentation
        run: |
          cp wiki-docs/*.md wiki/
          
      - name: Commit and push to wiki
        run: |
          cd wiki
          git config user.name "GitHub Actions"
          git config user.email "actions@github.com"
          git add *.md
          git commit -m "Auto-sync from main repo" || echo "No changes"
          git push
```

## Wiki Structure

After copying, your wiki will have this structure:

```
scala-au.id.cxd.math.wiki/
├── Home.md                          # Main landing page
├── Getting-Started.md               # Installation and basics
├── Quick-Start-Examples.md          # Code examples
├── Probability-Distributions.md     # Distribution guide
├── Statistical-Tests.md             # Hypothesis testing
├── Regression-Methods.md            # Regression techniques
├── Neural-Networks.md               # Deep learning
├── Text-Processing.md               # NLP and LSI
├── Multivariate-Analysis.md         # PCA, clustering
├── Data-Processing.md               # Data handling
├── Mathematical-Functions.md        # Special functions
├── API-Quick-Reference.md           # API lookup
├── Examples-Catalog.md              # Example programs
├── Contributing.md                  # How to contribute
├── Architecture.md                  # Design overview
└── TODOs.md                         # Future work
```

## Customizing Wiki

### Sidebar

Create `_Sidebar.md` for navigation:

```markdown
### Documentation

- [Home](Home)
- [Getting Started](Getting-Started)
- [Quick Start](Quick-Start-Examples)

### Core Guides

- [Probability Distributions](Probability-Distributions)
- [Statistical Tests](Statistical-Tests)
- [Regression Methods](Regression-Methods)
- [Neural Networks](Neural-Networks)

### Reference

- [API Quick Reference](API-Quick-Reference)
- [Examples Catalog](Examples-Catalog)
- [Contributing](Contributing)
```

### Footer

Create `_Footer.md`:

```markdown
---
[Home](Home) | [Getting Started](Getting-Started) | [GitHub](https://github.com/cxd/scala-au.id.cxd.math) | [API Docs](https://cxd.github.io/scala-au.id.cxd.math/latest/math/api/index.html)
```

## Maintaining the Wiki

### Updating Documentation

After making changes to `wiki-docs/` in the main repository:

```bash
# Sync changes
./sync-wiki.sh

# Or manually
cd scala-au.id.cxd.math.wiki
cp ../scala-au.id.cxd.math/wiki-docs/*.md ./
git add *.md
git commit -m "Update documentation"
git push origin master
```

### Best Practices

1. **Keep source in main repo**: Maintain wiki source in `wiki-docs/`
2. **Single source of truth**: Always edit in main repo, then sync to wiki
3. **Version control**: Track wiki changes in main repo
4. **Review before sync**: Check changes before pushing to wiki
5. **Test links**: Verify internal links work after copying

## Troubleshooting

### Wiki not accessible

- Check if Wiki is enabled in repository settings
- Verify you have push access to the repository

### Links not working

- GitHub wiki links don't need `.md` extension
- Use `[Link Text](Page-Name)` not `[Link Text](Page-Name.md)`
- Update links when copying if needed

### Images not displaying

- Upload images to wiki repository
- Use relative paths: `![Alt text](images/diagram.png)`
- Or use absolute URLs to images in main repo

### Permission denied

```bash
# Use HTTPS with token
git clone https://YOUR_TOKEN@github.com/cxd/scala-au.id.cxd.math.wiki.git

# Or use SSH
git clone git@github.com:cxd/scala-au.id.cxd.math.wiki.git
```

## Alternative: Documentation in Main Repo

If you prefer to keep documentation in the main repository:

1. **Keep `wiki-docs/` in main repo**
2. **Add link in README**: Point to `wiki-docs/` directory
3. **No sync needed**: Users read directly from repo
4. **Version controlled**: Documentation versioned with code

Example README addition:
```markdown
## Documentation

Browse the [comprehensive documentation](wiki-docs/Home.md) in this repository.

Or visit the [GitHub Wiki](https://github.com/cxd/scala-au.id.cxd.math/wiki) (if synced).
```

## Summary

Choose the method that works best for you:

- **Manual copy**: Best for one-time setup
- **Script**: Good for regular updates
- **GitHub Actions**: Best for automatic sync
- **Main repo only**: Simplest, no sync needed

All methods are valid - pick what fits your workflow!

---

[← Back to Home](Home.md)
