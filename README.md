# Maximum Bid Model

Simple, client-side maximum-bid model for two September 2026 Texas sheriff-sale properties:

- 701 Skillman St., Dallas, TX
- 2718 S Pace Bend Rd., Spicewood, TX

## Preview

```sh
python3 -m http.server 8765 --bind 127.0.0.1
```

Open `http://127.0.0.1:8765/`.

## Model

- One maximum-bid answer per property
- Plain green bidding room and red stop line
- Summary, Analysis, and independent Opinion views
- Editable sale price, cushion, return goal, and costs
- Expandable itemized repair allowances with include, rename, amount, uncertainty, add, and remove controls
- Fixed $5,000 slider steps for sale price and repair budget; typed fields retain exact values
- Monthly discounted cash flow
- Live downside sensitivities, market evidence, and direct primary-source links
- Accessible tap, hover, and keyboard information popovers across model terms
- Public educational-use disclaimer

Inputs are editable estimates. Public comp evidence labels asking prices and estimate bands instead of presenting them as completed sale prices.

This model is educational discussion material, not investment, legal, tax, appraisal, engineering, inspection, title, or bidding advice.

## GitHub Pages

The site has no build step or external runtime dependency. Publish the repository root with GitHub Pages.
