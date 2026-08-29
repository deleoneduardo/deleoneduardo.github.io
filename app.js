(() => {
  const DATA = window.SHERIFF_DATA;
  const withSales = DATA.counties.filter(c => c.items.length > 0);
  const noSales = DATA.counties.filter(c => c.items.length === 0);

  const state = {
    countyKey: withSales[0].key,
    index: 0,
    minBid: 0,
  };

  const $ = id => document.getElementById(id);
  const fmtMoney = n => n == null ? '—' : '$' + Math.round(n).toLocaleString('en-US');

  function county() {
    return withSales.find(c => c.key === state.countyKey);
  }

  function filteredItems() {
    return county().items.filter(it => (it.minBidNum ?? 0) >= state.minBid);
  }

  // ---- header tabs ----
  function renderTabs() {
    const wrap = $('countyTabs');
    wrap.innerHTML = '';
    for (const c of withSales) {
      const b = document.createElement('button');
      b.type = 'button';
      b.setAttribute('aria-pressed', String(c.key === state.countyKey));
      const metroCity = c.metro.replace(', TX', '');
      b.innerHTML = `<span>${metroCity}<span class="state-suffix">, TX</span></span><small>${c.county}</small>`;
      b.addEventListener('click', () => {
        state.countyKey = c.key;
        state.index = 0;
        render();
      });
      wrap.appendChild(b);
    }
  }

  // ---- current property card ----
  function renderCard() {
    const c = county();
    const items = filteredItems();
    const total = items.length;
    if (state.index >= total) state.index = Math.max(0, total - 1);
    const it = items[state.index];

    const dateTxt = c.saleDate
      ? new Date(c.saleDate).toLocaleDateString('en-US', { month: 'long', day: 'numeric', year: 'numeric' })
      : '';
    $('auctionLine').textContent = `${c.county} · near ${c.metro} — Tax sale ${dateTxt}${c.saleTime ? ' · ' + c.saleTime : ''}`;

    const stamp = $('stamp');
    const chip = $('statusChip');

    if (!it) {
      $('propertyAddress').textContent = 'No properties above this amount';
      $('propertyFacts').textContent = 'Lower the minimum bid filter to see more properties in ' + c.county + '.';
      $('minBidBig').textContent = '—';
      $('bidRule').textContent = '';
      chip.hidden = true;
      stamp.hidden = true;
      $('qAdjudged').textContent = '—';
      $('qRatio').textContent = '—';
      $('qAccount').textContent = '—';
      $('qCause').textContent = '';
      $('pagerCount').textContent = '0 of 0';
      $('prevBtn').disabled = true;
      $('nextBtn').disabled = true;
      return;
    }

    $('propertyAddress').textContent = it.address;
    $('propertyFacts').textContent = [it.city, it.cause ? 'Cause ' + it.cause : '']
      .filter(Boolean).join(' · ');
    $('minBidBig').textContent = it.minBid || fmtMoney(it.minBidNum);
    $('bidRule').textContent = 'Opening bid required at auction.';

    // status chip
    const st = it.status;
    chip.hidden = false;
    chip.classList.remove('is-bad', 'is-neutral');
    if (st.kind === 'scheduled') {
      chip.textContent = 'Scheduled for auction ' + dateTxt;
    } else if (st.kind === 'sold') {
      chip.classList.add('is-bad');
      chip.textContent = 'Sold' + (st.buyer ? ' to ' + st.buyer : '') + (st.amountText ? ' for ' + st.amountText : '');
    } else if (st.kind === 'struck') {
      chip.classList.add('is-neutral');
      chip.textContent = 'Struck off to taxing entity' + (st.amountText ? ' at ' + st.amountText : '');
    } else if (st.kind === 'paid') {
      chip.classList.add('is-neutral');
      chip.textContent = st.label + ' — removed from sale';
    } else if (st.kind === 'canceled' || st.kind === 'pulled') {
      chip.classList.add('is-bad');
      chip.textContent = st.label;
    } else {
      chip.classList.add('is-neutral');
      chip.textContent = st.label;
    }

    // tilted stamp
    if (st.kind === 'sold' && st.amountText) {
      stamp.hidden = false;
      stamp.classList.remove('is-grey');
      $('stampText').textContent = 'Sold for ' + st.amountText;
    } else if (st.kind === 'struck') {
      stamp.hidden = false;
      stamp.classList.add('is-grey');
      $('stampText').textContent = 'Struck off';
    } else if (st.kind === 'canceled' || st.kind === 'pulled' || st.kind === 'paid') {
      stamp.hidden = false;
      stamp.classList.add('is-grey');
      $('stampText').textContent = st.kind === 'paid' ? 'Off the sale' : 'Canceled';
    } else {
      stamp.hidden = true;
    }

    $('qAdjudged').textContent = it.adjudged || '—';
    $('qRatio').textContent = (it.adjudgedNum && it.minBidNum)
      ? Math.round(100 * it.minBidNum / it.adjudgedNum) + '%'
      : '—';
    $('qAccount').textContent = it.account || '—';
    $('qCause').textContent = it.cause ? 'Cause ' + it.cause : '';

    $('pagerCount').textContent = (state.index + 1) + ' of ' + total + ' in ' + c.county;
    $('prevBtn').disabled = state.index <= 0;
    $('nextBtn').disabled = state.index >= total - 1;
  }

  // ---- list ----
  function renderList() {
    const c = county();
    const items = filteredItems();
    const wrap = $('propList');
    wrap.innerHTML = '';
    if (!items.length) {
      const d = document.createElement('div');
      d.className = 'empty-state';
      d.textContent = 'No ' + c.county + ' properties with a minimum bid above ' + fmtMoney(state.minBid) + '.';
      wrap.appendChild(d);
    }
    items.forEach((it, i) => {
      const row = document.createElement('button');
      row.type = 'button';
      row.className = 'prop-row' + (i === state.index ? ' is-current' : '');
      const flag = flagFor(it.status);
      row.innerHTML =
        `<span class="addr">${it.address}<small>${it.city || ''}</small></span>` +
        (flag ? `<span class="row-flag ${flag.cls}">${flag.txt}</span>` : '') +
        `<span class="bid">${it.minBid || fmtMoney(it.minBidNum)}</span>`;
      row.addEventListener('click', () => {
        state.index = i;
        render();
        document.getElementById('property').scrollIntoView({ behavior: 'smooth' });
      });
      wrap.appendChild(row);
    });

    const shown = items.length, all = c.items.length;
    $('filterSummary').textContent = shown + ' of ' + all + ' properties shown';
    $('sourceNote').innerHTML = 'Source: <a href="https://' + c.site + '" rel="noopener" target="_blank">' + c.site + '</a> — captured ' + DATA.generated + '.';
  }

  function flagFor(st) {
    if (st.kind === 'sold') return { cls: 'sold', txt: 'Sold' };
    if (st.kind === 'struck') return { cls: 'struck', txt: 'Struck off' };
    if (st.kind === 'paid') return { cls: 'off', txt: 'Paid' };
    if (st.kind === 'canceled') return { cls: 'off', txt: 'Canceled' };
    if (st.kind === 'pulled') return { cls: 'off', txt: 'No bids' };
    if (st.kind === 'arrangement') return { cls: 'off', txt: 'Payment plan' };
    return null;
  }

  // ---- no-sale counties ----
  function renderNoSale() {
    const wrap = $('noSaleList');
    wrap.innerHTML = '';
    for (const c of noSales) {
      const d = document.createElement('div');
      d.className = 'no-sale-item';
      d.innerHTML = '<strong>' + c.metro + '</strong>' + c.county;
      wrap.appendChild(d);
    }
  }

  // ---- filter wiring ----
  const input = $('minBidInput');
  const range = $('minBidRange');

  function setMinBid(v, from) {
    state.minBid = Math.max(0, Number(v) || 0);
    if (from !== 'input') input.value = state.minBid;
    if (from !== 'range') range.value = Math.min(state.minBid, Number(range.max));
    state.index = 0;
    render();
  }
  input.addEventListener('input', () => setMinBid(input.value, 'input'));
  range.addEventListener('input', () => setMinBid(range.value, 'range'));

  // ---- pager ----
  $('prevBtn').addEventListener('click', () => { if (state.index > 0) { state.index--; render(); } });
  $('nextBtn').addEventListener('click', () => {
    if (state.index < filteredItems().length - 1) { state.index++; render(); }
  });
  document.addEventListener('keydown', e => {
    if (e.target.tagName === 'INPUT') return;
    if (e.key === 'ArrowLeft') $('prevBtn').click();
    if (e.key === 'ArrowRight') $('nextBtn').click();
  });

  function render() {
    renderTabs();
    renderCard();
    renderList();
  }

  renderNoSale();
  render();
})();
