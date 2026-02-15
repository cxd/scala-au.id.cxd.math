# MkDocs Usage Guide

This guide explains how to use MkDocs to build and serve the documentation locally.

## What is MkDocs?

MkDocs is a fast, simple static site generator that's geared towards building project documentation. It takes your markdown documentation files and generates a beautiful, searchable static website.

## Installation

### Prerequisites

- Python 3.7 or higher
- pip (Python package manager)

### Install MkDocs and Dependencies

```bash
# Install MkDocs with Material theme
pip install mkdocs-material

# Install additional plugins
pip install mkdocs-minify-plugin
```

Or install all at once with the recommended setup:

```bash
pip install mkdocs-material mkdocs-minify-plugin pymdown-extensions
```

### Alternative: Using a Virtual Environment

```bash
# Create virtual environment
python -m venv venv

# Activate (Linux/Mac)
source venv/bin/activate

# Activate (Windows)
venv\Scripts\activate

# Install dependencies
pip install mkdocs-material mkdocs-minify-plugin pymdown-extensions
```

## Usage

### Serve Documentation Locally

Build and serve the documentation with live reload:

```bash
mkdocs serve
```

This will start a development server at `http://127.0.0.1:8000/` where you can preview the documentation. The server automatically reloads when you make changes to the markdown files.

**Options**:
```bash
# Serve on a different port
mkdocs serve -a localhost:8080

# Serve on all network interfaces
mkdocs serve -a 0.0.0.0:8000

# Enable verbose mode
mkdocs serve --verbose
```

### Build Static Site

Generate static HTML files:

```bash
mkdocs build
```

This creates a `site/` directory containing the complete static website.

**Options**:
```bash
# Build with verbose output
mkdocs build --verbose

# Clean build (remove old files first)
mkdocs build --clean

# Build to custom directory
mkdocs build --site-dir custom-output
```

### Deploy to GitHub Pages

Deploy documentation directly to GitHub Pages:

```bash
mkdocs gh-deploy
```

This builds the documentation and pushes it to the `gh-pages` branch of your repository.

**Options**:
```bash
# Deploy with custom commit message
mkdocs gh-deploy --message "Update documentation"

# Force push (use with caution)
mkdocs gh-deploy --force
```

## Configuration

The documentation is configured in `mkdocs.yml` at the repository root. Key sections:

### Site Information

```yaml
site_name: scala-au.id.cxd.math Documentation
site_description: 'A comprehensive Scala library...'
site_author: 'cxd'
site_url: 'https://cxd.github.io/scala-au.id.cxd.math/'
```

### Theme Configuration

The documentation uses the Material for MkDocs theme with:
- Light and dark mode support
- Navigation tabs and sections
- Search functionality
- Code syntax highlighting
- Math equation rendering (MathJax)

### Navigation Structure

The navigation menu is defined in the `nav` section:

```yaml
nav:
  - Home: Home.md
  - Getting Started:
    - Installation & Setup: Getting-Started.md
    - Quick Start Examples: Quick-Start-Examples.md
  # ... more sections
```

## Customization

### Adding Pages

1. Create a new markdown file in `wiki-docs/`
2. Add it to the `nav` section in `mkdocs.yml`:

```yaml
nav:
  - Your New Page: Your-New-Page.md
```

### Custom Styling

Custom CSS is located in `wiki-docs/stylesheets/extra.css`. Add your styles there.

### Custom JavaScript

Custom JavaScript is in `wiki-docs/javascripts/mathjax.js`. The MathJax configuration is already set up for rendering mathematical formulas.

### Adding Plugins

Install the plugin and add it to the `plugins` section in `mkdocs.yml`:

```yaml
plugins:
  - search
  - your-plugin-name
```

## Math Equations

You can include mathematical equations using LaTeX syntax:

**Inline math**:
```markdown
The formula \(E = mc^2\) is famous.
```

**Display math**:
```markdown
The normal distribution PDF is:

\[
f(x) = \frac{1}{\sigma\sqrt{2\pi}} e^{-\frac{(x-\mu)^2}{2\sigma^2}}
\]
```

## Code Blocks

Syntax highlighting is automatic:

````markdown
```scala
val normal = Normal(mu = 0.0, sigma = 1.0)
val prob = normal.cdf(1.96)
```
````

## Admonitions (Info Boxes)

Create styled info boxes:

```markdown
!!! note
    This is a note.

!!! warning
    This is a warning.

!!! tip
    This is a tip.
```

## Tips

### Live Preview

Keep `mkdocs serve` running while editing for instant preview of changes.

### Validation

MkDocs will warn you about:
- Broken internal links
- Missing files
- Invalid configuration

### Search

The search functionality is automatically built and works offline in the static site.

### Mobile-Friendly

The Material theme is fully responsive and works great on mobile devices.

## Troubleshooting

### Port Already in Use

If port 8000 is busy:
```bash
mkdocs serve -a localhost:8001
```

### Build Errors

Check for:
- Invalid YAML in `mkdocs.yml`
- Missing markdown files referenced in `nav`
- Broken internal links

Enable verbose mode for details:
```bash
mkdocs build --verbose
```

### Theme Not Found

Install Material theme:
```bash
pip install mkdocs-material
```

### Math Not Rendering

Ensure MathJax is loaded in `mkdocs.yml`:
```yaml
extra_javascript:
  - javascripts/mathjax.js
  - https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js
```

## CI/CD Integration

### GitHub Actions

Create `.github/workflows/docs.yml`:

```yaml
name: Deploy Documentation

on:
  push:
    branches:
      - main
    paths:
      - 'wiki-docs/**'
      - 'mkdocs.yml'

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      
      - name: Setup Python
        uses: actions/setup-python@v2
        with:
          python-version: 3.x
      
      - name: Install dependencies
        run: |
          pip install mkdocs-material
          pip install mkdocs-minify-plugin
      
      - name: Deploy to GitHub Pages
        run: mkdocs gh-deploy --force
```

## Resources

- [MkDocs Documentation](https://www.mkdocs.org/)
- [Material for MkDocs](https://squidfunk.github.io/mkdocs-material/)
- [PyMdown Extensions](https://facelessuser.github.io/pymdown-extensions/)

## Quick Reference

```bash
# Install
pip install mkdocs-material mkdocs-minify-plugin

# Preview locally
mkdocs serve

# Build static site
mkdocs build

# Deploy to GitHub Pages
mkdocs gh-deploy

# Get help
mkdocs --help
mkdocs serve --help
mkdocs build --help
```

---

[← Back to Documentation Home](Home.md)
