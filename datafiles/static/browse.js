const d = document;

const initialParams = new URL(d.location).searchParams;
// This parameter is named 'terms' because it is from before filters were
// introduced. But we will parse it as a normal search string (including filters)
const initialSearchQuery = initialParams.has('terms') ? initialParams.get('terms') : ''
d.querySelector("#searchQuery").value = initialSearchQuery;

class Model {
  page = 0
  numberOfResults = 0
  column = 'default'
  direction = 'ascending'
  searchQuery = initialSearchQuery
  flipDirection() {
    if (this.direction === 'ascending') {
      return ['ascending', this.direction = 'descending'];
    } else {
      return ['descending', this.direction = 'ascending'];
    }
  }
}

const state = new Model();

addEventListener('popstate', async (evt) => {
  if (evt.state === null) {
    return;
  }
  state.page = evt.state.page;
  state.column = evt.state.column;
  state.direction = evt.state.direction;
  state.searchQuery = evt.state.searchQuery;
  d.querySelector("#searchQuery").value = evt.state.searchQuery;
  await refresh();
});

const get = () => new Promise((resolve,reject) => {
    const obj =
      {   page: state.page
        , sortColumn: state.column
        , sortDirection: state.direction
        , searchQuery: state.searchQuery
      };
    const fetchOptions =
      {   method: 'POST'
        , headers: {'content-type': 'application/json'}
        , body: JSON.stringify(obj)
      };
    fetch('/packages/search', fetchOptions).then(async (response) => {
      if (!response.ok) {
        const el = d.querySelector("#fatalError");
        el.style.display = "block";
        const err = await response.text();
        el.textContent = "Error with Hackage server: " + err;
        console.log(obj);
        reject(new Error("fetch failed: " + err));
      } else {
        resolve(response.json());
      }
    });
});

const createName = (nameDict) => {
  const name = d.createElement("td");
  const nameLink = d.createElement("a");
  nameLink.setAttribute("href", nameDict.uri);
  nameLink.appendChild(d.createTextNode(nameDict.display));
  name.appendChild(nameLink);
  return name;
}

const createSimpleText = (text) => {
  const el = d.createElement("td");
  el.appendChild(d.createTextNode(text));
  return el;
}

// Used with renderUser and renderTag results from backend
const createCommaList = (arr) => {
  const ul = d.createElement("ul");
  ul.classList.add("commaList");
  for (const dict of arr) {
    const li = d.createElement("li");
    const a = d.createElement("a");
    a.setAttribute("href", dict.uri);
    a.appendChild(d.createTextNode(dict.display));
    li.appendChild(a);
    ul.appendChild(li);
  }
  return ul;
}

const createTags = (tagsArr) => {
  const el = d.createElement("td");
  if (tagsArr === []) {
    return el;
  }
  el.appendChild(d.createTextNode("("));
  const ul = createCommaList(tagsArr);
  el.appendChild(ul);
  el.appendChild(d.createTextNode(")"));
  return el;
};

const createLastUpload = (lastUploadISO8601) => {
  const el = d.createElement("td");
  const date = lastUploadISO8601.substr(0, "0000-00-00".length);
  el.setAttribute("title", new Date(lastUploadISO8601).toLocaleString());
  el.classList.add("lastUpload");
  el.appendChild(d.createTextNode(date));
  return el;
};

const createMaintainers = (maintainersArr) => {
  const el = d.createElement("td");
  if (maintainersArr === []) {
    return el;
  }
  const ul = createCommaList(maintainersArr);
  el.appendChild(ul);
  return el;
};

const replaceRows = (response) => {
  const l = d.querySelector("#listing");
  l.replaceChildren();
  for (const row of response.pageContents) {
    const tr = d.createElement("tr");
    tr.appendChild(createName(row.name));
    tr.appendChild(createSimpleText(row.downloads));
    tr.appendChild(createSimpleText(row.votes));
    tr.appendChild(createSimpleText(row.description));
    tr.appendChild(createTags(row.tags));
    tr.appendChild(createLastUpload(row.lastUpload));
    tr.appendChild(createMaintainers(row.maintainers));
    tr.appendChild(createSimpleText(row.packageRank));
    l.appendChild(tr);
  }
};

const removeSortIndicator = () => {
  // No column is actually visible for the default sort mode,
  // so there is nothing to do in that case.
  if (state.column !== 'default') {
    const columnHeader = d.querySelector("#arrow-" + state.column);
    columnHeader.removeAttribute("aria-sort");
    const oldClasses = columnHeader.classList;
    oldClasses.remove('ascending');
    oldClasses.remove('descending');
  }
}

export const sort = async (column) => {
  if (state.column === column) {
    const [oldCls, newCls] = state.flipDirection();
    const columnHeader = d.querySelector("#arrow-" + column);
    const classes = columnHeader.classList;
    classes.toggle(oldCls);
    classes.toggle(newCls);
    columnHeader.setAttribute("aria-sort", newCls);
  } else {
    removeSortIndicator();

    state.direction = 'ascending';
    state.column = column;

    // Add sort indicator on new column
    const columnHeader = d.querySelector("#arrow-" + column);
    columnHeader.classList.add("ascending");
    columnHeader.setAttribute("aria-sort", "ascending");
  }
  state.page = 0;
  await refresh();
};

const pageSize = 50; // make sure it is kept in sync with backend

const pageAvailable = (page) => {
  if (page < 0) return false;
  if (page === 0) return true;
  return page * pageSize < state.numberOfResults;
}

const changePage = async (candidate) => {
  if (!pageAvailable(candidate)) {
    return;
  }
  state.page = candidate;
  history.pushState(state, d.title);
  await refresh();
  scrollTo(0, d.body.scrollHeight);
};

const createIndexIndicator = () => {
  const el = d.createElement("div");
  const minIdx = state.page * pageSize + 1;
  let maxIdx = (state.page + 1) * pageSize;
  maxIdx = Math.min(maxIdx, state.numberOfResults);
  let fullMsg;
  if (state.numberOfResults === 0) {
    fullMsg = "No results found.";
  } else {
    const entriesText = state.numberOfResults === 1 ? "entry" : "entries";
    fullMsg = `Showing ${minIdx} to ${maxIdx} of ${state.numberOfResults} ${entriesText}`;
  }
  el.appendChild(d.createTextNode(fullMsg));
  return el;
};

const refresh = async () => {
  const res = await get();
  state.numberOfResults = res.numberOfResults;
  replaceRows(res);
  const container = d.querySelector("#paginatorContainer");
  container.replaceChildren();
  container.appendChild(createIndexIndicator());
  container.appendChild(createPaginator());
  if (state.searchQuery.trim() === "") {
    d.querySelector("#browseFooter").style.display = "none";
  } else {
    d.querySelector("#browseFooter").style.display = "block";
    const url = new URL(hoogleNoParam);
    url.searchParams.set("hoogle", state.searchQuery);
    d.querySelector("#hoogleLink").setAttribute("href", url);
  }
};

export const submitSearch = async (evt) => {
  if (evt) evt.preventDefault();
  state.searchQuery = d.querySelector("#searchQuery").value;
  removeSortIndicator();
  state.column = 'default';
  state.direction = 'ascending';
  state.page = 0;

  const url = new URL(d.location);
  url.searchParams.set('terms', state.searchQuery);
  history.pushState(state, d.title, url);

  await refresh();
};

const createPageLink = (num) => {
  const a = d.createElement("a");
  if (state.page == num) a.classList.add("current");
  a.setAttribute("href", "#");
  a.addEventListener('click', (evt) => {
    evt.preventDefault();
    changePage(num);
  });
  a.appendChild(d.createTextNode(num + 1));
  return a;
};

const createPrevNext = (prevNextNum, cond, txt) => {
  const el = d.createElement(cond ? "span" : "a");
  el.setAttribute("href", "#");
  el.addEventListener('click', (evt) => {
    evt.preventDefault();
    changePage(prevNextNum);
  });
  if (cond) el.classList.add("disabled");
  el.appendChild(d.createTextNode(txt));
  return el;
};

const createEllipsis = () => {
  const el = d.createElement("span");
  el.innerHTML = "&hellip;";
  return el;
};

const createPaginator = () => {
  const maxPage = maxAvailablePage(state.numberOfResults);

  const pag = d.createElement("div");
  pag.classList.add("paginator");
  pag.appendChild(createPrevNext(state.page - 1, state.page === 0, "Previous"));
  // note that page is zero-indexed
  if (maxPage <= 4) {
    // No ellipsis
    for (let i = 0; i <= maxPage; i++) {
      pag.appendChild(createPageLink(i));
    }
  } else if (state.page <= 3) {
    // One ellipsis, at the end
    for (let i = 0; i <= 4; i++) {
      pag.appendChild(createPageLink(i));
    }
    pag.appendChild(createEllipsis());
    pag.appendChild(createPageLink(maxPage));
  } else if (state.page + 3 >= maxPage) {
    // One ellipsis, at the start
    pag.appendChild(createPageLink(0));
    pag.appendChild(createEllipsis());
    for (let i = maxPage - 4; i <= maxPage; i++) {
      pag.appendChild(createPageLink(i));
    }
  } else {
    // Two ellipses, at both ends
    pag.appendChild(createPageLink(0));
    pag.appendChild(createEllipsis());
    for (let i = state.page - 1; i <= state.page + 1; i++) {
      pag.appendChild(createPageLink(i));
    }
    pag.appendChild(createEllipsis());
    pag.appendChild(createPageLink(maxPage));
  }
  const isNowOnLastPage = state.page === maxPage;
  pag.appendChild(createPrevNext(state.page + 1, isNowOnLastPage, "Next"));

  return pag;
};

const maxAvailablePage = (numberOfResults) => {
  if (numberOfResults === 0) numberOfResults++;
  return Math.floor((numberOfResults - 1) / pageSize);
};

const hoogleNoParam = "https://hoogle.haskell.org";

let expanded = false;

export const toggleAdvanced = () => {
  if (expanded) {
    d.querySelector("#toggleAdvanced").setAttribute("aria-expanded", "false");
    d.querySelector("#chevron").innerHTML = "&#x25B8;";
    d.querySelector("#advancedForm").style.display = "none";
  } else {
    d.querySelector("#toggleAdvanced").setAttribute("aria-expanded", "true");
    d.querySelector("#chevron").innerHTML = "&#x25BE;";
    d.querySelector("#advancedForm").style.display = "block";
  }
  expanded = !expanded;
};

export const appendDeprecated = async (evt) => {
  if (evt) evt.preventDefault();
  d.querySelector("#searchQuery").value += " (deprecated:any)";
  await submitSearch();
};

const isNonNegativeFloatString = (n) => {
  // If there is a decimal separator, digits before it are required.
  const parsed = parseFloat(n.match(/^\d+(\.\d+)?$/));
  return parsed >= 0;
};

export const validateAgeOfLastUL = () => {
  const el = d.querySelector("#advAgeLastUL");
  const duration = el.value.trim();
  if (duration === ""
        || !(["d", "w", "m", "y"].includes(duration.substr(-1, 1)))
        || !isNonNegativeFloatString(duration.substr(0, duration.length - 1))) {
    el.setCustomValidity("Must be positive and end in d(ay), w(eek), m(onth) or y(ear)");
    return false;
  }
  el.setCustomValidity("");
  return duration;
};

export const appendAgeOfLastUL = async (evt) => {
  if (evt) evt.preventDefault();
  const maybeDuration = validateAgeOfLastUL();
  if (maybeDuration === false) {
    return;
  }
  const duration = maybeDuration;
  d.querySelector("#searchQuery").value += ` (ageOfLastUpload < ${duration})`;
  await submitSearch();
};

export const validateTag = () => {
  const el = d.querySelector("#advTag");
  const tag = el.value.trim();
  if (tag === "" || !(/^[a-z0-9]+$/i.test(tag))) {
    el.setCustomValidity("Tag cannot be empty and must be alphanumeric and ASCII");
    return false;
  }
  el.setCustomValidity("");
  return tag;
}

export const appendTag = async (evt) => {
  if (evt) evt.preventDefault();
  const maybeTag = validateTag();
  if (maybeTag === false) {
    return;
  }
  const tag = maybeTag;
  d.querySelector("#searchQuery").value += ` (tag:${tag})`;
  await submitSearch();
};

export const appendRating = async (evt) => {
  if (evt) evt.preventDefault();
  const rating = d.querySelector("#advRatingSlider").value;
  d.querySelector("#searchQuery").value += ` (rating >= ${rating})`;
  await submitSearch();
};

// Avoid top-level await since as of Apr 2022 it only has 84% penetration:
// https://caniuse.com/?search=top%20level%20await
// We don't need the result anyway.
refresh();
