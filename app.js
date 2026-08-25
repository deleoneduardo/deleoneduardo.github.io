"use strict";

(function maximumBidApp(root) {
  const STORAGE_KEY = "maximum-bid-model-v3";
  const VERSION = 3;
  const VIEW_KEYS = ["summary", "analysis", "opinion"];
  const FIELD_BOUNDS = {
    expectedSale: [0, 10000000],
    contingencyPct: [0, 100],
    returnPct: [0, 100],
    sellingCostPct: [0, 30],
    diligence: [0, 10000000],
    monthlyCarry: [0, 1000000],
    holdMonths: [1, 36],
    depositPct: [0, 100]
  };
  const money = new Intl.NumberFormat("en-US", {
    style: "currency",
    currency: "USD",
    maximumFractionDigits: 0
  });
  const exactMoney = new Intl.NumberFormat("en-US", {
    style: "currency",
    currency: "USD",
    minimumFractionDigits: 2,
    maximumFractionDigits: 2
  });
  const number = new Intl.NumberFormat("en-US", { maximumFractionDigits: 1 });

  const macroSources = [
    ["Federal Reserve · July 29, 2026", "Fed funds 3.50%–3.75%; inflation remains elevated", "https://www.federalreserve.gov/newsevents/pressreleases/monetary20260729a.htm"],
    ["Freddie Mac · August 20, 2026", "30-year mortgage average 6.65%", "https://www.freddiemac.com/pmms"],
    ["FHFA · 2026 Q1", "Repeat-sales house-price change by metro", "https://www.fhfa.gov/document/d/hpi/fhfa-house-price-index-report-2026q1"],
    ["Texas REALTORS · 2026 Q2", "Median price, sales, marketing time, and inventory", "https://www.texasrealestate.com/wp-content/uploads/2026_Q2_Texas_Quarterly_Housing_Report.pdf"],
    ["BLS Producer Price Index · July 2026", "Final-demand construction prices +5.2% year over year", "https://www.bls.gov/news.release/ppi.htm"],
    ["Texas Tax Code §34.21", "Non-homestead redemption framework", "https://statutes.capitol.texas.gov/Docs/TX/pdf/TX.34.pdf"]
  ];

  const HELP_BY_LABEL = {
    "Maximum bid": "Highest modeled purchase price that still funds costs and selected return goal. It is a ceiling, not a target.",
    "Start": "County's estimated minimum opening bid. Auction bidding may begin or clear differently.",
    "Safe bidding room": "Difference between modeled maximum bid and estimated auction minimum.",
    "Stop": "Hard modeled ceiling. Higher bidding reduces return below selected goal.",
    "Too high": "Price above model's maximum bid under current assumptions.",
    "Room to bid": "Maximum bid minus estimated auction minimum.",
    "5% deposit at maximum": "Five percent of modeled maximum bid. Deposit becomes part of purchase price, not an added cost.",
    "Yearly return goal": "Annual compound hurdle used to discount future cash flows back to today's dollars.",
    "Why this can be a good deal": "Entry-basis evidence only. It does not guarantee condition, resale price, or profit.",
    "Land + house": "Subject includes modeled acreage and existing improvement; value is not counted twice.",
    "Nearby empty lot": "Nearby land listing used as context. Asking price is not a completed sale.",
    "Price at maximum": "Modeled maximum bid divided by subject acreage.",
    "Expected sale": "Editable estimate of post-work resale price before selling costs.",
    "Maximum purchase": "Highest purchase price allowed by current discounted-cash-flow assumptions.",
    "Repairs + cushion": "Itemized repair budget plus percentage contingency.",
    "Change estimates": "Edit assumptions here; maximum bid, analysis, and opinion recalculate immediately.",
    "Expected sale price": "Estimated post-work sale price. Slider moves by $5,000; typed field accepts exact values.",
    "Repairs": "Sum of enabled repair allowances before extra contingency.",
    "Total repair budget": "Sum of enabled line items. Total slider adds or removes only unallocated budget.",
    "Extra repair cushion": "Percentage reserve added above itemized repairs for unknown scope or price changes.",
    "More costs": "Secondary assumptions: selling fees, diligence, ownership costs, and holding time.",
    "Sale fees": "Percentage removed from resale proceeds for brokerage, closing, concessions, and related exit costs.",
    "Inspection + legal": "Cash allowance for diligence, records, inspections, surveys, and professional review.",
    "Monthly ownership cost": "Taxes, insurance, utilities, security, maintenance, and other carrying cash per month.",
    "Months until sale": "Modeled time until exit proceeds arrive. Longer time lowers present value and adds carrying cost.",
    "New repair item": "Add missing scope so it becomes part of total repair budget.",
    "Allowance": "Provisional cash budget until contractor quote or inspection supplies better evidence.",
    "How maximum is found": "Discounted-cash-flow model subtracts present value of repairs, diligence, carrying costs, and required return from net sale proceeds.",
    "Sale money − costs − return needed = maximum bid": "Plain-language form of model's present-value equation.",
    "Money after sale fees": "Expected sale price minus percentage selling costs.",
    "Minus return needed over time": "Gap between future net sale proceeds and present value at selected annual return goal.",
    "Minus repairs + cushion": "Present value of itemized repairs plus contingency, modeled near midpoint of hold.",
    "Minus inspection + legal": "Diligence allowance treated as immediate cash outflow.",
    "Minus ownership costs": "Present value of monthly carrying cash through modeled hold.",
    "Comparable properties and sources": "Links supporting subject facts, market context, and editable value assumptions.",
    "Underwriting analysis": "Current model mechanics, sensitivity, and evidence—not a promise of outcome.",
    "Auction start": "County's estimated minimum opening bid.",
    "All-in cash at maximum": "Maximum bid plus repairs, cushion, diligence, and nominal ownership costs.",
    "Net sale money": "Expected sale proceeds after modeled selling fees.",
    "Modeled profit": "Net sale money minus total nominal modeled cash at maximum bid.",
    "Deal mechanics": "Core cash movements and return conversion behind maximum bid.",
    "Value case": "Evidence supporting possible spread between purchase basis and conservative resale.",
    "Repair exposure": "Largest editable repair allowances and contingency burden.",
    "Time and capital": "How cash-auction funding, redemption period, mortgage market, and hold duration affect value.",
    "What moves maximum bid": "One-variable downside tests; all other assumptions stay unchanged.",
    "Expected sale price falls $25,000": "Recalculates cap after a $25,000 resale-value decline.",
    "Repairs rise $25,000": "Recalculates cap after $25,000 added repair scope, plus contingency.",
    "Sale takes 3 more months": "Recalculates cap with three added months of discounting and ownership costs.",
    "Yearly return goal rises 5 points": "Recalculates cap using annual hurdle five percentage points higher.",
    "Current market signals": "Latest sourced rates, prices, supply, jobs, and construction-cost indicators used in critique.",
    "30-year mortgage": "Average buyer mortgage rate. Auction acquisition is cash; rate affects resale affordability.",
    "Fed funds": "Federal Reserve policy-rate target. It affects broader financing conditions, not auction cash funding directly.",
    "Local HPI YoY": "FHFA repeat-sales House Price Index change over prior year for local metro. Negative means prices fell.",
    "Inventory": "Months needed to sell active housing supply at current pace. Higher value usually favors buyers.",
    "Jobs YoY": "Local payroll-employment change from same month one year earlier; growth can support housing demand.",
    "Construction prices": "Producer-price change for final-demand construction; rising costs pressure repair budgets.",
    "Strongest case": "Best evidence supporting investment thesis under current assumptions.",
    "What model may be fooling itself about": "Assumptions most likely to create false confidence or overstated margin.",
    "Macro pressure": "External forces affecting exit demand, pricing, repair costs, and liquidity.",
    "Bid discipline": "Rules preventing auction competition from overriding modeled economics.",
    "Conditions that can kill deal": "Property-specific failures capable of erasing expected return.",
    "Facts behind opinion": "Primary and property-level sources supporting current critique.",
    "Disclaimer": "Scope and limits for readers who encounter this public model."
  };

  const INFO_TARGET_SELECTOR = [
    "#maximumBidLabel",
    ".road-point > span",
    ".road-safe > span",
    ".road-high",
    ".quick-numbers > div > span",
    ".section-heading h2",
    ".proof-card > span",
    ".word-formula",
    ".math-list > div > span",
    ".page-lead > .answer-label",
    ".opinion-hero > .answer-label",
    ".analysis-metric > span",
    ".analysis-card h3",
    ".sensitivity-row > span",
    ".market-card > span",
    ".opinion-card h3",
    "#disclaimerHeading",
    "[data-info]"
  ].join(",");

  const defaults = {
    version: VERSION,
    active: "spicewood",
    view: "summary",
    properties: {
      dallas: {
        city: "Dallas",
        address: "701 Skillman St., Dallas, TX 75214",
        auctionLine: "Dallas County · September 1, 2026 · 9:00 AM CT",
        facts: "3 beds · 2 baths · 2,164 ft² house · 7,748 ft² lot",
        auctionMinimum: 105868.09,
        priorProposal: 280000,
        adjudgedValue: 568410,
        expectedSale: 640000,
        contingencyPct: 15,
        returnPct: 27,
        sellingCostPct: 8,
        diligence: 30000,
        monthlyCarry: 2700,
        holdMonths: 6,
        depositPct: 5,
        market: {
          mortgageRate: 6.65,
          fedRange: "3.50%–3.75%",
          hpiYoY: -1.27,
          medianPrice: 395145,
          medianChange: -1.2,
          salesChange: 4.4,
          inventoryMonths: 4.5,
          daysOnMarket: 57,
          jobGrowth: 1.3
        },
        repairItems: [
          item("cleanup", "Cleanup, demolition, haul-off", 10000, "Medium"),
          item("permits", "Permits, plans, inspections", 10000, "Medium"),
          item("historic", "Historic review / material match (conditional)", 20000, "High"),
          item("structure", "Foundation and structural work", 25000, "High"),
          item("roof", "Roof, gutters, flashing", 25000, "Medium"),
          item("windows", "Windows, doors, exterior envelope", 10000, "Medium"),
          item("hvac", "HVAC / air conditioning and ductwork", 15000, "Medium"),
          item("electrical", "Electrical service, panel, wiring", 15000, "High"),
          item("plumbing", "Plumbing, water heater, sewer", 15000, "High"),
          item("kitchen", "Kitchen", 30000, "Medium"),
          item("bathrooms", "Bathrooms", 20000, "Medium"),
          item("interior", "Flooring, drywall, paint, trim", 15000, "Medium"),
          item("exterior", "Exterior, fencing, landscaping", 10000, "Medium"),
          item("environmental", "Mold, asbestos, pest allowance", 10000, "High"),
          item("appliances", "Appliances and fixtures", 10000, "Low"),
          item("driveway", "Driveway, garage, site repairs", 10000, "Medium")
        ],
        sources: [
          ["Dallas County sale information", "Official auction source", "https://www.dallascounty.org/departments/tax/sheriff-sales.php"],
          ["Dallas County Appraisal District", "2,164 ft² and assessor context", "https://www.dallascad.org/AcctDetailRes.aspx?ID=00000182032000000"],
          ["Dallas BLS · July 2026", "Jobs +1.3% year over year", "https://www.bls.gov/eag/eag.tx_dallas_msa.htm"],
          ["723 Skillman St.", "$827k–$947k public band; restored and superior", "https://www.har.com/homedetail/723-skillman-st-dallas-tx-75214/8230674"],
          ["724 Lowell St.", "$717k–$827k public band; close size and era", "https://www.har.com/homedetail/724-lowell-st-dallas-tx-75214/1320934"],
          ["711 Glendale St.", "$370k–$420k public band; renovation condition", "https://www.redfin.com/TX/Dallas/711-Glendale-St-75214/home/30805759"]
        ]
      },
      spicewood: {
        city: "Austin",
        address: "2718 S Pace Bend Rd., Spicewood, TX 78669",
        auctionLine: "Travis County · September 1, 2026 · 10:00 AM CT",
        facts: "1.32 riverfront acres · 2,132 ft² house · about 48 minutes from Austin",
        auctionMinimum: 158642.06,
        priorProposal: 430000,
        adjudgedValue: 943741,
        expectedSale: 720000,
        contingencyPct: 15,
        returnPct: 27,
        sellingCostPct: 8.5,
        diligence: 40000,
        monthlyCarry: 3600,
        holdMonths: 6,
        depositPct: 5,
        acres: 1.3215,
        nearbyLotAsk: 420000,
        nearbyLotAcres: 1.3554,
        market: {
          mortgageRate: 6.65,
          fedRange: "3.50%–3.75%",
          hpiYoY: -6.88,
          hpiQuarter: -3.82,
          medianPrice: 439900,
          medianChange: -0.8,
          salesChange: 10,
          inventoryMonths: 5.6,
          daysOnMarket: 68,
          jobGrowth: 1.7
        },
        repairItems: [
          item("cleanup", "Cleanup, demolition, haul-off", 10000, "Medium"),
          item("permits", "Permits, plans, floodplain review", 10000, "High"),
          item("structure", "Foundation and structural work", 25000, "High"),
          item("roof", "Roof, gutters, flashing", 25000, "Medium"),
          item("windows", "Windows, doors, exterior envelope", 10000, "Medium"),
          item("hvac", "HVAC / air conditioning and ductwork", 20000, "Medium"),
          item("electrical", "Electrical service, panel, wiring", 15000, "High"),
          item("plumbing", "Interior plumbing", 15000, "Medium"),
          item("well", "Well, pump, water treatment", 15000, "High"),
          item("septic", "Septic system inspection and repair", 20000, "High"),
          item("drainage", "Drainage, erosion, flood resilience", 20000, "High"),
          item("dock", "Dock, boat lift, shoreline work", 40000, "High"),
          item("kitchen", "Kitchen", 30000, "Medium"),
          item("bathrooms", "Bathrooms", 15000, "Medium"),
          item("interior", "Flooring, drywall, paint, trim", 15000, "Medium"),
          item("exterior", "Decks, exterior, landscaping", 10000, "Medium"),
          item("environmental", "Mold, pest, environmental work", 10000, "High"),
          item("appliances", "Appliances and fixtures", 10000, "Low"),
          item("access", "Driveway, access, site repairs", 10000, "Medium"),
          item("safety", "Fire safety, alarms, code items", 5000, "Low")
        ],
        sources: [
          ["Travis County sale information", "Official auction and deed information", "https://tax-office.traviscountytx.gov/properties/foreclosed"],
          ["Travis Central Appraisal District", "1.3215 acres, 2,132 ft², and assessor context", "https://travis.prodigycad.com/property-detail/356263"],
          ["Austin BLS · July 2026", "Jobs +1.7% year over year", "https://www.bls.gov/eag/eag.tx_austin_msa.htm"],
          ["Subject property on Zillow", "$721,800 public estimate; model rounds down to $720,000", "https://www.zillow.com/homedetails/2718-S-Pace-Bend-Rd-Spicewood-TX-78669/29519175_zpid/"],
          ["2706 S Pace Bend Rd.", "$420k ask for 1.3554 empty acres; active ask, not sale", "https://www.realtor.com/realestateandhomes-detail/2706-Pace-Bend-Rd-S_Spicewood_TX_78669_M95926-89448"],
          ["Lake Travis reservoir data", "Water level changes over time", "https://waterdatafortexas.org/reservoirs/individual/travis"]
        ]
      }
    }
  };

  let state = load();
  let tooltipSequence = 0;

  function item(id, name, cost, uncertainty, enabled = true) {
    return { id, name, cost, uncertainty, enabled };
  }

  function clone(value) {
    return JSON.parse(JSON.stringify(value));
  }

  function finite(value, fallback = 0) {
    const parsed = Number(value);
    return Number.isFinite(parsed) ? parsed : fallback;
  }

  function clamp(value, minimum, maximum) {
    return Math.min(maximum, Math.max(minimum, finite(value, minimum)));
  }

  function boundedField(field, value, fallback = 0) {
    const [minimum, maximum] = FIELD_BOUNDS[field] || [-Infinity, Infinity];
    return clamp(finite(value, fallback), minimum, maximum);
  }

  function escapeHtml(value) {
    return String(value ?? "")
      .replaceAll("&", "&amp;")
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll('"', "&quot;")
      .replaceAll("'", "&#039;");
  }

  function alignInfoTips() {
    root.document.querySelectorAll(".info-wrap").forEach((wrap) => {
      wrap.classList.remove("align-left", "align-right");
      const rect = wrap.getBoundingClientRect();
      if (rect.left < 150) wrap.classList.add("align-left");
      else if (root.innerWidth - rect.right < 280) wrap.classList.add("align-right");
    });
  }

  function popoverIsOpen(popover) {
    if (!popover?.matches) return false;
    try {
      return popover.matches(":popover-open");
    } catch {
      return false;
    }
  }

  function positionInfoTip(wrap) {
    const button = wrap?.querySelector("[data-info-button]");
    const popover = wrap?.querySelector(".info-popover");
    if (!button || !popover || !wrap.classList.contains("is-open") || !popoverIsOpen(popover)) return;
    const buttonRect = button.getBoundingClientRect();
    const popoverRect = popover.getBoundingClientRect();
    const gutter = 10;
    const gap = 8;
    const maxLeft = Math.max(gutter, root.innerWidth - popoverRect.width - gutter);
    const left = clamp(buttonRect.left + buttonRect.width / 2 - popoverRect.width / 2, gutter, maxLeft);
    const below = buttonRect.bottom + gap;
    const above = buttonRect.top - popoverRect.height - gap;
    const top = below + popoverRect.height <= root.innerHeight - gutter || above < gutter
      ? clamp(below, gutter, Math.max(gutter, root.innerHeight - popoverRect.height - gutter))
      : above;
    popover.style.right = "auto";
    popover.style.bottom = "auto";
    popover.style.transform = "none";
    popover.style.left = `${Math.round(left)}px`;
    popover.style.top = `${Math.round(top)}px`;
  }

  function openInfoTip(wrap, pinned = false) {
    if (!wrap) return;
    const popover = wrap.querySelector(".info-popover");
    const button = wrap.querySelector("[data-info-button]");
    wrap.classList.remove("is-focus-dismissed");
    wrap.classList.add("is-open");
    wrap.classList.toggle("is-pinned", pinned || wrap.classList.contains("is-pinned"));
    button?.setAttribute("aria-expanded", "true");
    if (popover?.showPopover && !popoverIsOpen(popover)) {
      try { popover.showPopover(); } catch { /* CSS fallback remains available. */ }
    }
    root.requestAnimationFrame(() => positionInfoTip(wrap));
  }

  function closeInfoTip(wrap) {
    if (!wrap) return;
    const popover = wrap.querySelector(".info-popover");
    wrap.classList.remove("is-open", "is-pinned");
    wrap.querySelector("[data-info-button]")?.setAttribute("aria-expanded", "false");
    if (popover?.hidePopover && popoverIsOpen(popover)) {
      try { popover.hidePopover(); } catch { /* Element may have been replaced during render. */ }
    }
  }

  function closeInfoTips(except = null) {
    root.document.querySelectorAll(".info-wrap.is-open").forEach((wrap) => {
      if (wrap === except) return;
      closeInfoTip(wrap);
    });
  }

  function attachInfoTip(target) {
    if (target.dataset.infoReady === "true") return;
    const label = (target.dataset.infoLabel || target.textContent || "Information").replace(/\s+/g, " ").trim();
    const help = target.dataset.info || HELP_BY_LABEL[label] || (label.startsWith("Independent finance opinion")
      ? "Dated judgment based on current inputs and sources—not appraisal or guaranteed outcome."
      : null);
    if (!help) return;
    target.dataset.infoReady = "true";
    tooltipSequence += 1;
    const tooltipId = `infoTip${tooltipSequence}`;
    const wrap = root.document.createElement("span");
    wrap.className = "info-wrap";
    const button = root.document.createElement("button");
    button.type = "button";
    button.className = "info-tip";
    button.dataset.infoButton = "";
    button.setAttribute("aria-label", `Explain ${label.toLowerCase()}`);
    button.setAttribute("aria-describedby", tooltipId);
    button.setAttribute("aria-expanded", "false");
    button.textContent = "i";
    const popover = root.document.createElement("span");
    popover.className = "info-popover";
    popover.id = tooltipId;
    popover.setAttribute("role", "tooltip");
    if (popover.showPopover) popover.setAttribute("popover", "manual");
    popover.textContent = help;
    wrap.append(button, popover);
    target.append(root.document.createTextNode("\u00a0"), wrap);
  }

  function enhanceInfoTips() {
    root.document.querySelectorAll(INFO_TARGET_SELECTOR).forEach(attachInfoTip);
    root.requestAnimationFrame(alignInfoTips);
  }

  function load() {
    const clean = clone(defaults);
    try {
      const saved = JSON.parse(root.localStorage.getItem(STORAGE_KEY) || "null");
      if (!saved || saved.version !== VERSION) return clean;
      if (saved.active in clean.properties) clean.active = saved.active;
      if (VIEW_KEYS.includes(saved.view)) clean.view = saved.view;
      Object.keys(clean.properties).forEach((key) => {
        const incoming = saved.properties?.[key];
        if (!incoming) return;
        Object.keys(clean.properties[key]).forEach((field) => {
          if (typeof clean.properties[key][field] === "number" && Number.isFinite(incoming[field])) {
            clean.properties[key][field] = boundedField(field, incoming[field], clean.properties[key][field]);
          }
        });
        if (Array.isArray(incoming.repairItems)) {
          clean.properties[key].repairItems = incoming.repairItems
            .filter((entry) => entry && typeof entry.name === "string")
            .map((entry, index) => ({
              id: String(entry.id || `saved-${index}`),
              name: entry.name.slice(0, 70),
              cost: Math.max(0, finite(entry.cost)),
              uncertainty: ["Low", "Medium", "High"].includes(entry.uncertainty) ? entry.uncertainty : "Medium",
              enabled: entry.enabled !== false
            }));
        }
      });
      return clean;
    } catch (_error) {
      return clean;
    }
  }

  function save() {
    try {
      root.localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
    } catch (_error) {
      // Model remains usable without browser storage.
    }
  }

  function repairTotal(property) {
    return property.repairItems.reduce(
      (total, entry) => total + (entry.enabled ? Math.max(0, finite(entry.cost)) : 0),
      0
    );
  }

  function calculate(property, overrides = {}) {
    const months = boundedField("holdMonths", overrides.holdMonths ?? property.holdMonths, property.holdMonths);
    const returnPct = boundedField("returnPct", overrides.returnPct ?? property.returnPct, property.returnPct);
    const annualReturn = Math.max(-.99, returnPct / 100);
    const monthlyReturn = Math.pow(1 + annualReturn, 1 / 12) - 1;
    const expectedSale = boundedField("expectedSale", overrides.expectedSale ?? property.expectedSale, property.expectedSale);
    const repairs = Math.max(0, finite(overrides.repairs, repairTotal(property)));
    const sellingCostPct = boundedField("sellingCostPct", overrides.sellingCostPct ?? property.sellingCostPct, property.sellingCostPct);
    const contingencyPct = boundedField("contingencyPct", overrides.contingencyPct ?? property.contingencyPct, property.contingencyPct);
    const diligence = boundedField("diligence", overrides.diligence ?? property.diligence, property.diligence);
    const monthlyCarry = boundedField("monthlyCarry", overrides.monthlyCarry ?? property.monthlyCarry, property.monthlyCarry);
    const saleFees = expectedSale * sellingCostPct / 100;
    const netSale = Math.max(0, expectedSale - saleFees);
    const saleDiscount = Math.pow(1 + monthlyReturn, months);
    const presentSale = netSale / saleDiscount;
    const repairCash = repairs * (1 + contingencyPct / 100);
    const repairDiscount = Math.pow(1 + monthlyReturn, months / 2);
    const presentRepairs = repairCash / repairDiscount;
    let presentCarry = 0;
    const wholeMonths = Math.floor(months);
    for (let month = 1; month <= wholeMonths; month += 1) {
      presentCarry += monthlyCarry / Math.pow(1 + monthlyReturn, month);
    }
    const partialMonth = months - wholeMonths;
    if (partialMonth > 0) {
      presentCarry += monthlyCarry * partialMonth / Math.pow(1 + monthlyReturn, months);
    }
    const maximumBid = Math.max(0, presentSale - presentRepairs - diligence - presentCarry);
    const bidRoom = maximumBid - property.auctionMinimum;
    const deposit = maximumBid * property.depositPct / 100;
    const nominalCarry = monthlyCarry * months;
    const allInCash = maximumBid + repairCash + diligence + nominalCarry;
    const profitAtMaximum = netSale - allInCash;
    const periodReturn = Math.pow(1 + annualReturn, months / 12) - 1;
    const requiredPresentSaleAtMinimum = property.auctionMinimum + presentRepairs + diligence + presentCarry;
    const saleNeededAtMinimum = requiredPresentSaleAtMinimum * saleDiscount / Math.max(.01, 1 - sellingCostPct / 100);
    const availablePresentRepairAtMinimum = Math.max(0, presentSale - property.auctionMinimum - diligence - presentCarry);
    const repairFeasibleAtMinimum = presentSale - property.auctionMinimum - diligence - presentCarry >= 0;
    const repairLimitAtMinimum = availablePresentRepairAtMinimum * repairDiscount / (1 + contingencyPct / 100);
    return {
      months,
      returnPct,
      expectedSale,
      repairs,
      saleFees,
      netSale,
      presentSale,
      returnBuffer: netSale - presentSale,
      repairCash,
      presentRepairs,
      presentCarry,
      nominalCarry,
      diligence,
      maximumBid,
      bidRoom,
      deposit,
      allInCash,
      profitAtMaximum,
      periodReturn,
      saleNeededAtMinimum,
      saleHeadroom: expectedSale - saleNeededAtMinimum,
      repairFeasibleAtMinimum,
      repairLimitAtMinimum,
      repairHeadroom: repairLimitAtMinimum - repairs
    };
  }

  function proofMarkup(key, property, result) {
    if (key === "spicewood") {
      const maxPerAcre = result.maximumBid / property.acres;
      const askPerAcre = property.nearbyLotAsk / property.nearbyLotAcres;
      const discount = 1 - maxPerAcre / askPerAcre;
      return {
        cards: [
          ["Land + house", "1.32 acres", "Riverfront land plus 2,132 ft² house."],
          ["Nearby empty lot", money.format(property.nearbyLotAsk), `${property.nearbyLotAcres.toFixed(2)} acres, four parcels away.`],
          ["Price at maximum", `${money.format(maxPerAcre)}/acre`, `${Math.max(0, discount * 100).toFixed(0)}% below nearby land asking price per acre.`]
        ],
        note: "Nearby $420,000 figure is asking price, not completed sale. House value is not added twice."
      };
    }
    const purchaseDiscount = 1 - result.maximumBid / property.expectedSale;
    return {
      cards: [
        ["Expected sale", money.format(property.expectedSale), "Inside nearby fixer-to-restored public value range."],
        ["Maximum purchase", money.format(result.maximumBid), `${Math.max(0, purchaseDiscount * 100).toFixed(0)}% below expected sale price.`],
        ["Repairs + cushion", money.format(result.repairCash), `${property.contingencyPct}% extra is included after itemized work.`]
      ],
      note: "Nearby figures are public listing or estimate bands, not guaranteed sale prices."
    };
  }

  function sourceMarkup(sources) {
    return sources.map(([name, note, url]) => `
      <a href="${escapeHtml(url)}" target="_blank" rel="noopener">
        <span><strong>${escapeHtml(name)}</strong><small>${escapeHtml(note)}</small></span>
        <span aria-hidden="true">↗</span>
      </a>`).join("");
  }

  function repairMarkup(property) {
    return property.repairItems.map((entry) => `
      <div class="repair-row ${entry.enabled ? "" : "is-excluded"}" data-repair-id="${escapeHtml(entry.id)}">
        <div class="repair-item-copy">
          <div class="repair-toggle-name">
            <input type="checkbox" data-repair-toggle="${escapeHtml(entry.id)}" ${entry.enabled ? "checked" : ""} aria-label="Include ${escapeHtml(entry.name)}">
            <textarea class="repair-name" rows="1" maxlength="70" data-repair-name="${escapeHtml(entry.id)}" aria-label="Repair item name">${escapeHtml(entry.name)}</textarea>
          </div>
        </div>
        <label class="repair-field repair-cost-field">
          <span class="repair-field-label">Allowance</span>
          <span class="number-box"><span>$</span><input type="number" min="0" step="5000" value="${Math.round(entry.cost)}" data-repair-cost="${escapeHtml(entry.id)}" aria-label="${escapeHtml(entry.name)} allowance"></span>
        </label>
        <button class="repair-remove" type="button" data-repair-remove="${escapeHtml(entry.id)}" aria-label="Remove ${escapeHtml(entry.name)}"><span class="repair-remove-symbol" aria-hidden="true">×</span><span class="repair-remove-text">Remove</span></button>
      </div>`).join("");
  }

  function resizeRepairNames(scope = root.document) {
    scope.querySelectorAll?.(".repair-name").forEach((control) => {
      control.style.height = "auto";
      control.style.height = `${Math.max(40, control.scrollHeight + 2)}px`;
    });
  }

  function resizeOpenRepairNames() {
    const editor = root.document.getElementById("repairEditor");
    if (editor?.open) resizeRepairNames(editor);
  }

  function analysisMarkup(key, property, result) {
    const topRepairs = property.repairItems
      .filter((entry) => entry.enabled)
      .sort((a, b) => b.cost - a.cost)
      .slice(0, 3)
      .map((entry) => `${entry.name} ${money.format(entry.cost)}`)
      .join("; ");
    const periodPct = result.periodReturn * 100;
    const mechanics = `At maximum bid, total modeled cash is ${money.format(result.allInCash)}. Net sale proceeds are ${money.format(result.netSale)}. A ${result.returnPct}% yearly goal equals ${periodPct.toFixed(1)}% over ${number.format(result.months)} months.`;
    const valueCase = key === "spicewood"
      ? `Value comes from entry basis: 1.3215 acres, water frontage, and house. Nearby 1.3554-acre empty land asks ${money.format(property.nearbyLotAsk)}, but that is an active ask—not proof of sale. Model uses ${money.format(property.expectedSale)} as editable exit.`
      : `Value comes from buying far below modeled resale, then executing renovation. Dallas has deeper comparable evidence and better urban liquidity. Restored houses do not prove this subject reaches ${money.format(property.expectedSale)}.`;
    const repairExposure = `Itemized repairs total ${money.format(result.repairs)}; ${property.contingencyPct}% cushion raises cash repair reserve to ${money.format(result.repairCash)}. Largest allowances: ${topRepairs}. Every item remains editable.`;
    const capital = `Auction purchase requires cash; the 6.65% mortgage rate matters only when selling to the next buyer. Texas generally gives 180 days to redeem a non-homestead tax-sale property; redemption generally repays qualifying amounts plus a 25% premium. Model uses ${number.format(result.months)} months, with timing shown as a sensitivity.`;
    return [
      ["Deal mechanics", mechanics],
      ["Value case", valueCase],
      ["Repair exposure", repairExposure],
      ["Time and capital", capital]
    ];
  }

  function opinionData(key, property, result) {
    const investable = result.bidRoom >= 0;
    const basis = investable ? result.maximumBid : property.auctionMinimum;
    const basisLabel = investable ? "maximum bid" : "auction start";
    const capShare = basis / property.adjudgedValue;
    const halfValueShare = basis / (property.adjudgedValue * .5);
    const cap = money.format(result.maximumBid);
    const priorGap = property.priorProposal - result.maximumBid;
    const priorDecision = priorGap > 0
      ? `Prior ${money.format(property.priorProposal)} proposal exceeds model by ${money.format(priorGap)}: reject.`
      : `Prior ${money.format(property.priorProposal)} proposal is inside model, but remains a ceiling—not a target.`;
    const repairAlternative = result.repairFeasibleAtMinimum
      ? `or repairs must fall to ${money.format(result.repairLimitAtMinimum)}`
      : "and even $0 repairs would not clear auction start";
    const noBid = `Current maximum is ${money.format(Math.abs(result.bidRoom))} below auction start. Holding other inputs steady, exit must reach ${money.format(result.saleNeededAtMinimum)} ${repairAlternative}.`;
    if (key === "spicewood") {
      return {
        title: investable ? "Best opportunity. Attractive land basis—not safe house flip." : "Not investable at current assumptions.",
        lead: investable ? `Pursue near auction floor. Hard stop: ${cap}. Edge comes from purchase basis, not hoped Austin appreciation.` : noBid,
        verdict: investable ? `Bid only below ${cap}` : "Do not bid",
        cards: [
          ["Strongest case", `At ${basisLabel}, buyer pays ${(capShare * 100).toFixed(0)}% of ${money.format(property.adjudgedValue)} adjudged value. Even cutting that value in half puts basis at ${(halfValueShare * 100).toFixed(0)}%. Acreage, house, and water frontage create asymmetric optionality.`, "is-case"],
          ["What model may be fooling itself about", `Nearby ${money.format(property.nearbyLotAsk)} land figure is an asking price. Austin repeat-sales prices fell 6.88% year over year. ${investable ? `Dock, septic, well, drainage, flood, access, or structure can erase only ${money.format(result.bidRoom)} of bidding room.` : "Current assumptions already leave no bidding room."}`, "is-risk"],
          ["Macro pressure", `Mortgage rates at 6.65% restrict exit-buyer budgets. Austin inventory is 5.6 months and marketing time 68 days. Jobs grew 1.7%, supporting demand, but not enough to justify appreciation in underwriting.`, ""],
          ["Bid discipline", `${priorDecision} Raise cap only when survey, waterfront rights, septic, structure, and dock evidence lower uncertainty. Competitive bidding is not new evidence.`, ""]
        ],
        kills: [
          "Waterfront, access, survey, or dock rights are unusable or restricted.",
          "Septic, well, foundation, drainage, erosion, or flood work exceeds allowances.",
          "Conservative closed-sale evidence cannot support exit value.",
          `Renovation or resale takes materially longer than ${number.format(result.months)} months.`
        ],
        final: investable ? `Spicewood is compelling below ${cap}. Above it, margin vanishes fast. Walk.` : `Do not bid. ${noBid}`
      };
    }
    return {
      title: investable ? "Good trade at disciplined price. Not extraordinary bargain." : "Not investable at current assumptions.",
      lead: investable ? `Pursue only below ${cap}. Dallas offers better liquidity; renovation execution owns the risk.` : noBid,
      verdict: investable ? `Bid only below ${cap}` : "Do not bid",
      cards: [
        ["Strongest case", `At ${basisLabel}, buyer pays ${(capShare * 100).toFixed(0)}% of ${money.format(property.adjudgedValue)} adjudged value. Dallas sales rose 4.4% and jobs 1.3%, giving a better resale-demand floor than a remote asset.`, "is-case"],
        ["What model may be fooling itself about", `Repair reserve reaches ${money.format(result.repairCash)}. Restored nearby listings do not prove subject exit. Foundation, electrical, plumbing, HVAC, moisture, historic review, or permit delay can turn paper spread into dead capital.`, "is-risk"],
        ["Macro pressure", `Dallas repeat-sales prices fell 1.27% year over year. Mortgage rates at 6.65% limit buyer budgets; inventory is 4.5 months and marketing time 57 days. Underwrite flat prices.`, ""],
        ["Bid discipline", `${priorDecision} Raise cap only if contractor evidence lowers repairs or conservative closed sales lift exit. Auction competition changes price—not value.`, ""]
      ],
      kills: [
        "Foundation, electrical, plumbing, HVAC, or moisture work exceeds allowances.",
        "Permit or conditional historic-review requirements expand cost or schedule.",
        "Closed comparable sales cannot support conservative exit value.",
        "Renovation and resale cannot finish near modeled holding period."
      ],
      final: investable
        ? (priorGap > 0
          ? `Dallas works below ${cap}. At ${money.format(property.priorProposal)}, investor absorbs old-house risk without enough payment. Walk.`
          : `Dallas works below ${cap}. Keep the prior proposal below that hard stop; auction momentum is not evidence.`)
        : `Do not bid. ${noBid}`
    };
  }

  function setText(id, value) {
    const element = root.document.getElementById(id);
    if (element) element.textContent = value;
  }

  function renderNavigation() {
    root.document.querySelectorAll("[data-property-tab]").forEach((button) => {
      button.setAttribute("aria-pressed", String(button.dataset.propertyTab === state.active));
    });
    root.document.querySelectorAll("[data-view-tab]").forEach((button) => {
      const active = button.dataset.viewTab === state.view;
      button.setAttribute("aria-selected", String(active));
      button.tabIndex = active ? 0 : -1;
    });
    root.document.querySelectorAll("[data-view-panel]").forEach((panel) => {
      panel.hidden = panel.dataset.viewPanel !== state.view;
    });
    const reset = root.document.querySelector("[data-reset]");
    if (reset) reset.setAttribute("aria-label", `Reset ${state.properties[state.active].city} numbers`);
  }

  function renderSummary(key, property, result, refreshControls = true) {
    setText("maximumBid", money.format(result.maximumBid));
    setText("auctionMinimum", exactMoney.format(property.auctionMinimum));
    setText("roadMaximum", money.format(result.maximumBid));
    setText("bidRoom", result.bidRoom >= 0 ? money.format(result.bidRoom) : `Short ${money.format(Math.abs(result.bidRoom))}`);
    setText("deposit", money.format(result.deposit));
    setText("returnGoal", `${property.returnPct.toFixed(0)}%`);
    setText("returnExample", `$100 aims to become ${money.format(100 * (1 + property.returnPct / 100))} after one year`);
    const status = root.document.getElementById("status");
    if (result.bidRoom >= 0) {
      status.classList.remove("no-room");
      status.textContent = `Good deal below ${money.format(result.maximumBid)}. Stop above it.`;
    } else {
      status.classList.add("no-room");
      status.textContent = result.repairFeasibleAtMinimum
        ? `No safe bid. Exit must reach ${money.format(result.saleNeededAtMinimum)}, or repairs must fall to ${money.format(result.repairLimitAtMinimum)}.`
        : `No safe bid. Exit must reach ${money.format(result.saleNeededAtMinimum)}; even $0 repairs would not clear auction start.`;
    }
    const proof = proofMarkup(key, property, result);
    root.document.getElementById("proofGrid").innerHTML = proof.cards.map(([label, value, note]) => `
      <article class="proof-card"><span>${escapeHtml(label)}</span><strong>${escapeHtml(value)}</strong><p>${escapeHtml(note)}</p></article>`).join("");
    setText("proofNote", proof.note);
    const totalRepairs = repairTotal(property);
    setText("repairSummaryTotal", money.format(totalRepairs));
    if (refreshControls) {
      root.document.querySelectorAll("[data-field]").forEach((input) => { input.value = property[input.dataset.field]; });
      root.document.querySelectorAll("[data-range]").forEach((input) => { input.value = property[input.dataset.range]; });
      const totalInput = root.document.querySelector("[data-repair-total]");
      const totalRange = root.document.querySelector("[data-repair-range]");
      if (totalInput) totalInput.value = totalRepairs;
      if (totalRange) {
        const nonUnallocated = property.repairItems
          .filter((entry) => entry.enabled && entry.id !== "unallocated")
          .reduce((sum, entry) => sum + entry.cost, 0);
        totalRange.min = Math.ceil(nonUnallocated / 5000) * 5000;
        totalRange.value = totalRepairs;
      }
      root.document.getElementById("repairList").innerHTML = repairMarkup(property);
      resizeRepairNames(root.document.getElementById("repairList"));
    }
    setText("netSale", money.format(result.netSale));
    setText("returnBuffer", `− ${money.format(result.returnBuffer)}`);
    setText("repairPV", `− ${money.format(result.presentRepairs)}`);
    setText("diligencePV", `− ${money.format(property.diligence)}`);
    setText("carryPV", `− ${money.format(result.presentCarry)}`);
    setText("mathMaximum", money.format(result.maximumBid));
    root.document.getElementById("sourceList").innerHTML = sourceMarkup(property.sources);
  }

  function renderAnalysis(key, property, result) {
    setText("analysisTitle", `${property.city}: what must be true`);
    setText("analysisLead", `Maximum bid is ${money.format(result.maximumBid)} because model prices resale, every repair allowance, carrying time, selling cost, and ${property.returnPct}% yearly return goal. It does not assume appreciation.`);
    const metrics = [
      ["Maximum bid", money.format(result.maximumBid)],
      ["Auction start", exactMoney.format(property.auctionMinimum)],
      ["All-in cash at maximum", money.format(result.allInCash)],
      ["Net sale money", money.format(result.netSale)],
      ["Modeled profit", money.format(result.profitAtMaximum)]
    ];
    root.document.getElementById("analysisMetrics").innerHTML = metrics.map(([label, value]) => `
      <div class="analysis-metric"><span>${escapeHtml(label)}</span><strong>${escapeHtml(value)}</strong></div>`).join("");
    root.document.getElementById("analysisNarrative").innerHTML = analysisMarkup(key, property, result)
      .map(([title, copy]) => `<article class="analysis-card"><h3>${escapeHtml(title)}</h3><p>${escapeHtml(copy)}</p></article>`).join("");
    const sensitivity = [
      ["Expected sale price falls $25,000", calculate(property, { expectedSale: property.expectedSale - 25000 }).maximumBid],
      ["Repairs rise $25,000", calculate(property, { repairs: result.repairs + 25000 }).maximumBid],
      ["Sale takes 3 more months", calculate(property, { holdMonths: property.holdMonths + 3 }).maximumBid],
      ["Yearly return goal rises 5 points", calculate(property, { returnPct: property.returnPct + 5 }).maximumBid]
    ];
    root.document.getElementById("sensitivityList").innerHTML = sensitivity.map(([label, cap]) => {
      const change = cap - result.maximumBid;
      const capText = `${money.format(cap)} cap`;
      const changeText = `${change >= 0 ? "+" : "−"}${money.format(Math.abs(change))}`;
      return `<div class="sensitivity-row"><span>${escapeHtml(label)}</span><strong aria-label="Recalculated maximum bid: ${escapeHtml(money.format(cap))}">${escapeHtml(capText)}</strong><strong class="${change < 0 ? "negative" : ""}" aria-label="Change from current maximum: ${escapeHtml(changeText)}">${escapeHtml(changeText)}</strong></div>`;
    }).join("");
    const market = property.market;
    const marketCards = [
      ["30-year mortgage", `${market.mortgageRate.toFixed(2)}%`],
      ["Fed funds", market.fedRange],
      ["Local HPI YoY", `${market.hpiYoY.toFixed(2)}%`],
      ["Inventory", `${market.inventoryMonths} months`],
      ["Jobs YoY", `+${market.jobGrowth}%`],
      ["Construction prices", "+5.2% YoY"]
    ];
    root.document.getElementById("marketGrid").innerHTML = marketCards.map(([label, value]) => `
      <div class="market-card"><span>${escapeHtml(label)}</span><strong>${escapeHtml(value)}</strong></div>`).join("");
    root.document.getElementById("analysisSources").innerHTML = sourceMarkup([...macroSources, ...property.sources.slice(0, 3)]);
  }

  function renderOpinion(key, property, result) {
    const opinion = opinionData(key, property, result);
    root.document.getElementById("opinionHero").innerHTML = `
      <p class="answer-label">Independent finance opinion · August 25, 2026</p>
      <h2>${escapeHtml(opinion.title)}</h2>
      <p>${escapeHtml(opinion.lead)}</p>
      <span class="opinion-verdict" data-info="Model-driven hard stop under current assumptions. It is not a target bid." data-info-label="Opinion hard stop">${escapeHtml(opinion.verdict)}</span>`;
    root.document.getElementById("opinionGrid").innerHTML = opinion.cards.map(([title, copy, className]) => `
      <article class="opinion-card ${className}"><h3>${escapeHtml(title)}</h3><p>${escapeHtml(copy)}</p></article>`).join("");
    root.document.getElementById("killList").innerHTML = opinion.kills.map((entry) => `<li>${escapeHtml(entry)}</li>`).join("");
    setText("finalInstruction", opinion.final);
    root.document.getElementById("opinionSources").innerHTML = sourceMarkup([...macroSources, ...property.sources.slice(0, 4)]);
  }

  function render(options = {}) {
    const refreshControls = options.refreshControls !== false;
    const key = state.active;
    const property = state.properties[key];
    const result = calculate(property);
    renderNavigation();
    setText("auctionLine", property.auctionLine);
    setText("propertyAddress", property.address);
    setText("propertyFacts", property.facts);
    renderSummary(key, property, result, refreshControls);
    renderAnalysis(key, property, result);
    renderOpinion(key, property, result);
    enhanceInfoTips();
  }

  function updateField(field, rawValue, refreshControls = true) {
    const property = state.properties[state.active];
    if (!(field in property) || typeof property[field] !== "number") return;
    property[field] = boundedField(field, rawValue, property[field]);
    save();
    if (!refreshControls) {
      const fieldInput = root.document.querySelector(`[data-field="${field}"]`);
      const rangeInput = root.document.querySelector(`[data-range="${field}"]`);
      if (fieldInput) fieldInput.value = property[field];
      if (rangeInput) rangeInput.value = property[field];
    }
    render({ refreshControls });
  }

  function findRepair(id) {
    return state.properties[state.active].repairItems.find((entry) => entry.id === id);
  }

  function setRepairTotal(rawTarget, refreshControls = true) {
    const property = state.properties[state.active];
    const allocated = property.repairItems
      .filter((entry) => entry.enabled && entry.id !== "unallocated")
      .reduce((sum, entry) => sum + entry.cost, 0);
    const target = Math.max(allocated, finite(rawTarget, repairTotal(property)));
    let unallocated = property.repairItems.find((entry) => entry.id === "unallocated");
    if (!unallocated) {
      unallocated = item("unallocated", "Unallocated repair allowance", 0, "High");
      property.repairItems.push(unallocated);
    }
    unallocated.enabled = true;
    unallocated.cost = Math.max(0, target - allocated);
    save();
    if (!refreshControls) {
      const totalInput = root.document.querySelector("[data-repair-total]");
      const totalRange = root.document.querySelector("[data-repair-range]");
      if (totalInput) totalInput.value = repairTotal(property);
      if (totalRange) totalRange.value = repairTotal(property);
    }
    render({ refreshControls });
  }

  function setView(view, focus = false) {
    if (!VIEW_KEYS.includes(view)) return;
    state.view = view;
    save();
    render();
    if (focus) root.document.querySelector(`[data-view-tab="${view}"]`)?.focus();
  }

  root.document.addEventListener("click", (event) => {
    const infoButton = event.target.closest("[data-info-button]");
    if (infoButton) {
      event.preventDefault();
      event.stopPropagation();
      const wrap = infoButton.closest(".info-wrap");
      if (wrap.classList.contains("is-pinned")) {
        closeInfoTip(wrap);
      } else {
        closeInfoTips(wrap);
        openInfoTip(wrap, true);
      }
      return;
    }
    if (!event.target.closest(".info-wrap")) closeInfoTips();
    const propertyButton = event.target.closest("[data-property-tab]");
    if (propertyButton) {
      state.active = propertyButton.dataset.propertyTab;
      save();
      render();
      return;
    }
    const viewButton = event.target.closest("[data-view-tab]");
    if (viewButton) {
      setView(viewButton.dataset.viewTab);
      return;
    }
    if (event.target.closest("[data-reset]")) {
      const active = state.active;
      const view = state.view;
      state.properties[active] = clone(defaults.properties[active]);
      state.view = view;
      save();
      render();
      return;
    }
    if (event.target.closest("[data-edit-estimates]")) {
      setView("summary");
      root.document.getElementById("changeEstimates")?.scrollIntoView({ behavior: "smooth", block: "start" });
      return;
    }
    const remove = event.target.closest("[data-repair-remove]");
    if (remove) {
      const property = state.properties[state.active];
      property.repairItems = property.repairItems.filter((entry) => entry.id !== remove.dataset.repairRemove);
      save();
      render();
    }
  });

  root.document.addEventListener("input", (event) => {
    if (event.target.matches("[data-field]") && event.target.value !== "") updateField(event.target.dataset.field, event.target.value, false);
    if (event.target.matches("[data-range]")) updateField(event.target.dataset.range, event.target.value, false);
    if (event.target.matches("[data-repair-range]")) setRepairTotal(event.target.value, false);
    if (event.target.matches("[data-repair-cost]") && event.target.value !== "") {
      const entry = findRepair(event.target.dataset.repairCost);
      if (entry) {
        entry.cost = Math.max(0, finite(event.target.value, entry.cost));
        save();
        render({ refreshControls: false });
      }
    }
    if (event.target.matches("[data-repair-name]")) {
      const entry = findRepair(event.target.dataset.repairName);
      if (entry) {
        entry.name = event.target.value.slice(0, 70);
        save();
        resizeRepairNames(event.target.parentElement);
      }
    }
  });

  root.document.addEventListener("change", (event) => {
    if (event.target.matches("[data-repair-total]") && event.target.value !== "") {
      setRepairTotal(event.target.value);
    } else if (event.target.matches("[data-field], [data-range], [data-repair-range]")) {
      render();
    }
    if (event.target.matches("[data-repair-cost]")) {
      const entry = findRepair(event.target.dataset.repairCost);
      if (entry) {
        entry.cost = Math.max(0, finite(event.target.value, entry.cost));
        event.target.value = entry.cost;
        save();
        render({ refreshControls: false });
      }
    }
    if (event.target.matches("[data-repair-toggle]")) {
      const entry = findRepair(event.target.dataset.repairToggle);
      if (entry) {
        const repairId = event.target.dataset.repairToggle;
        entry.enabled = event.target.checked;
        save();
        render();
        root.document.querySelector(`[data-repair-toggle="${repairId}"]`)?.focus();
      }
    }
  });

  root.document.addEventListener("submit", (event) => {
    if (event.target.id !== "addRepairForm") return;
    event.preventDefault();
    const data = new FormData(event.target);
    const name = String(data.get("repairName") || "").trim();
    if (!name) return;
    const property = state.properties[state.active];
    const id = `custom-${Date.now()}-${Math.random().toString(36).slice(2, 7)}`;
    property.repairItems.push(item(
      id,
      name.slice(0, 70),
      Math.max(0, finite(data.get("repairCost"), 5000)),
      "Medium"
    ));
    save();
    render();
    root.document.getElementById("repairEditor").open = true;
    root.document.querySelector(`[data-repair-name="${id}"]`)?.focus();
  });

  root.document.addEventListener("keydown", (event) => {
    if (event.target.matches?.("[data-repair-name]") && event.key === "Enter") {
      event.preventDefault();
      event.target.blur();
      return;
    }
    if (event.key === "Escape") {
      closeInfoTips();
      const focusedInfo = root.document.activeElement?.closest?.("[data-info-button]");
      focusedInfo?.closest(".info-wrap")?.classList.add("is-focus-dismissed");
      return;
    }
    const activeTab = event.target.closest("[data-view-tab]");
    if (!activeTab) return;
    const current = VIEW_KEYS.indexOf(activeTab.dataset.viewTab);
    let next = current;
    if (event.key === "ArrowRight") next = (current + 1) % VIEW_KEYS.length;
    else if (event.key === "ArrowLeft") next = (current - 1 + VIEW_KEYS.length) % VIEW_KEYS.length;
    else if (event.key === "Home") next = 0;
    else if (event.key === "End") next = VIEW_KEYS.length - 1;
    else return;
    event.preventDefault();
    setView(VIEW_KEYS[next], true);
  });

  root.document.addEventListener("pointerover", (event) => {
    const infoButton = event.target.closest?.("[data-info-button]");
    if (!infoButton) return;
    const wrap = infoButton.closest(".info-wrap");
    if (wrap.contains(event.relatedTarget)) return;
    closeInfoTips(wrap);
    openInfoTip(wrap);
  });

  root.document.addEventListener("pointerout", (event) => {
    const wrap = event.target.closest?.(".info-wrap");
    if (!wrap || wrap.contains(event.relatedTarget) || wrap.classList.contains("is-pinned")) return;
    root.setTimeout(() => {
      if (!wrap.contains(root.document.activeElement) && !wrap.classList.contains("is-pinned")) closeInfoTip(wrap);
    }, 0);
  });

  root.document.addEventListener("focusin", (event) => {
    const wrap = event.target.closest?.(".info-wrap");
    if (!wrap) return;
    wrap.classList.remove("is-focus-dismissed");
    if (event.target.matches?.("[data-info-button]")) {
      closeInfoTips(wrap);
      openInfoTip(wrap);
    }
  });

  root.document.addEventListener("focusout", (event) => {
    const wrap = event.target.closest?.(".info-wrap");
    if (!wrap || wrap.classList.contains("is-pinned")) return;
    root.setTimeout(() => {
      if (!wrap.contains(root.document.activeElement) && !wrap.classList.contains("is-pinned")) closeInfoTip(wrap);
    }, 0);
  });

  root.MaximumBidModel = {
    defaults: clone(defaults),
    get state() { return state; },
    calculate: (key = state.active) => calculate(state.properties[key]),
    repairTotal: (key = state.active) => repairTotal(state.properties[key]),
    reset() {
      state = clone(defaults);
      save();
      render();
    }
  };

  if (root.document.readyState === "loading") {
    root.document.addEventListener("DOMContentLoaded", render, { once: true });
  } else {
    render();
  }
  function repositionInfoTips() {
    alignInfoTips();
    root.document.querySelectorAll(".info-wrap.is-open").forEach(positionInfoTip);
    resizeOpenRepairNames();
  }

  root.document.getElementById("repairEditor")?.addEventListener("toggle", () => {
    root.requestAnimationFrame(() => root.requestAnimationFrame(resizeOpenRepairNames));
  });
  root.addEventListener("resize", repositionInfoTips);
  root.document.addEventListener("scroll", () => root.requestAnimationFrame(repositionInfoTips), true);
})(window);
