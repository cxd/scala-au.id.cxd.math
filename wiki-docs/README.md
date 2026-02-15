# Wiki Documentation for scala-au.id.cxd.math

This directory contains comprehensive documentation for the scala-au.id.cxd.math library, structured as a wiki.

## 📖 Documentation Index

### Getting Started
- **[Home.md](Home.md)** - Main landing page with overview and navigation
- **[Getting-Started.md](Getting-Started.md)** - Installation, building, and first steps
- **[Quick-Start-Examples.md](Quick-Start-Examples.md)** - Jump right in with code examples

### Core Guides
- **[Probability-Distributions.md](Probability-Distributions.md)** - Complete guide to distributions (Normal, Binomial, etc.)
- **[Statistical-Tests.md](Statistical-Tests.md)** - Hypothesis testing and inference (ANOVA, normality tests, etc.)
- **[Regression-Methods.md](Regression-Methods.md)** - Linear regression, logistic regression, Bayesian methods
- **[Neural-Networks.md](Neural-Networks.md)** - Building and training neural networks

### Reference
- **[API-Quick-Reference.md](API-Quick-Reference.md)** - Quick lookup of common operations and classes
- **[Examples-Catalog.md](Examples-Catalog.md)** - Complete catalog of working examples

### Development
- **[Contributing.md](Contributing.md)** - Guidelines for contributing to the project
- **[Architecture.md](Architecture.md)** - Design patterns and architecture overview
- **[WIKI-SETUP.md](WIKI-SETUP.md)** - Instructions for copying to GitHub wiki

## 🎯 Purpose

This documentation serves multiple purposes:

1. **Comprehensive Reference** - Detailed guides for all library features
2. **Learning Resource** - Helps users understand statistical concepts
3. **Code Examples** - Working code snippets for every major feature
4. **Best Practices** - Recommendations for effective use
5. **Wiki-Ready** - Can be easily copied to GitHub wiki

## 🚀 How to Use

### Option 1: Browse in Repository

Simply browse the markdown files in this directory. GitHub renders them nicely.

**Start here**: [Home.md](Home.md)

### Option 2: Copy to GitHub Wiki

Follow the instructions in [WIKI-SETUP.md](WIKI-SETUP.md) to copy these files to your GitHub wiki.

**Quick method**:
```bash
# Clone the wiki repository
git clone https://github.com/cxd/scala-au.id.cxd.math.wiki.git

# Copy all documentation files
cp wiki-docs/*.md scala-au.id.cxd.math.wiki/

# Commit and push
cd scala-au.id.cxd.math.wiki
git add *.md
git commit -m "Add comprehensive documentation"
git push origin master
```

### Option 3: Generate Static Site

Use a static site generator like Jekyll or MkDocs to create a documentation website.

## 📊 Documentation Coverage

| Topic | File | Status |
|-------|------|--------|
| Home/Overview | Home.md | ✅ Complete |
| Getting Started | Getting-Started.md | ✅ Complete |
| Quick Examples | Quick-Start-Examples.md | ✅ Complete |
| Probability Distributions | Probability-Distributions.md | ✅ Complete |
| Statistical Tests | Statistical-Tests.md | ✅ Complete |
| Regression Methods | Regression-Methods.md | ✅ Complete |
| Neural Networks | Neural-Networks.md | ✅ Complete |
| API Reference | API-Quick-Reference.md | ✅ Complete |
| Examples Catalog | Examples-Catalog.md | ✅ Complete |
| Contributing | Contributing.md | ✅ Complete |
| Architecture | Architecture.md | ✅ Complete |
| Wiki Setup | WIKI-SETUP.md | ✅ Complete |

## 🔗 Cross-References

All documentation files are interconnected with links. Each page includes:
- Navigation links to related topics
- Back-to-home links at the bottom
- Cross-references to relevant examples
- Links to external resources

## ✨ Key Features

### 1. Comprehensive Coverage

Every major feature of the library is documented:
- All probability distributions (continuous and discrete)
- Statistical tests and inference methods
- Regression techniques (linear, logistic, Bayesian)
- Neural network construction and training
- Text processing and LSI
- Data preprocessing and transformations

### 2. Working Code Examples

Every guide includes complete, runnable code examples:
```scala
import au.id.cxd.math.probability.continuous.Normal

val normal = Normal(mu = 0.0, sigma = 1.0)
val prob = normal.cdf(1.96)  // P(X ≤ 1.96)
```

### 3. Best Practices

Guides include:
- ✅ Recommended approaches
- ❌ Common pitfalls to avoid
- 💡 Tips and tricks
- ⚠️ Important warnings

### 4. Quick Reference

The API Quick Reference provides instant lookup:
- Common operations
- Import statements
- Function signatures
- Quick examples

## 🛠️ Maintaining Documentation

### Updating Content

1. **Edit files in this directory** (`wiki-docs/`)
2. **Test links** - Ensure internal links work
3. **Review examples** - Verify code compiles
4. **Sync to wiki** (if using GitHub wiki) - See WIKI-SETUP.md

### Adding New Pages

1. Create new `.md` file in this directory
2. Add link in `Home.md` navigation
3. Add links in related pages
4. Update this README
5. Sync to wiki if needed

### Documentation Style Guide

- Use clear, descriptive headings
- Include code examples for every concept
- Add cross-references to related topics
- Use tables for structured information
- Include "See Also" sections
- Add emoji for visual scanning (sparingly)

## 📝 Markdown Conventions

### Code Blocks

```scala
// Scala code with syntax highlighting
val x = 1 + 2
```

### Links

```markdown
[Link text](Page-Name.md)  # For GitHub
[Link text](Page-Name)     # For GitHub Wiki (no .md extension)
```

### Headings

```markdown
# Page Title (H1)
## Major Section (H2)
### Subsection (H3)
#### Detail (H4)
```

### Lists

```markdown
- Bullet points
- Use hyphens

1. Numbered lists
2. Sequential numbering
```

### Emphasis

```markdown
**Bold text** for important concepts
*Italic text* for emphasis
`code` for inline code references
```

## 🎨 Visual Elements

### Emoji Usage

Use emoji to improve scannability:
- 📖 Documentation/Reading
- 🚀 Getting Started/Quick
- ✅ Correct/Good practice
- ❌ Incorrect/Bad practice
- ⚠️ Warning/Caution
- 💡 Tip/Insight
- 🔗 Link/Reference
- 📊 Data/Statistics

### Tables

Use tables for structured comparisons:

| Feature | Description | Example |
|---------|-------------|---------|
| Normal | Gaussian distribution | `Normal(0, 1)` |
| Binomial | Binary trials | `Binomial(10, 0.5)` |

## 🤝 Contributing to Documentation

Improvements to documentation are highly valued! See [Contributing.md](Contributing.md) for guidelines.

**Easy contributions**:
- Fix typos
- Clarify confusing sections
- Add more examples
- Improve code snippets
- Add cross-references

## 📧 Feedback

Found an issue with the documentation?
- Open an issue on GitHub
- Suggest improvements
- Submit corrections

## 🙏 Acknowledgments

This documentation was created to make the library more accessible and to help users learn statistical computing with Scala.

---

**Start exploring**: [Home.md](Home.md)

**View online**: [GitHub Wiki](https://github.com/cxd/scala-au.id.cxd.math/wiki) (if synced)
