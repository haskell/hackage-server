<p>To view all packages, submit the form with an empty search query.</p>
<form method="POST" action="/packages/noscript-search" enctype="multipart/form-data">
  <input type=hidden name="_method" value="POST">
  <input type=hidden name="_transform" value="form2json">
  <div>
    <label>
      Sort direction:
      <select name="sortDirection=%s">
        <option $if(ascending)$  selected $endif$ value=ascending>ascending</option>
        <option $if(descending)$ selected $endif$ value=descending>descending</option>
      </select>
    </label>
  </div>
  <div>
    <label>
      Sort column:
      <select name="sortColumn=%s">
        <option $if(default)$     selected $endif$ value=default>default</option>
        <option $if(name)$        selected $endif$ value=name>name</option>
        <option $if(downloads)$   selected $endif$ value=downloads>number of downloads</option>
        <option $if(rating)$      selected $endif$ value=rating>rating</option>
        <option $if(description)$ selected $endif$ value=description>description</option>
        <option $if(tags)$        selected $endif$ value=tags>tags</option>
        <option $if(lastUpload)$  selected $endif$ value=lastUpload>date of last upload</option>
        <option $if(maintainers)$ selected $endif$ value=maintainers>maintainers</option>
        <option $if(packageRank)$ selected $endif$ value=packageRank>packageRank</option>
      </select>
    </label>
  </div>
  <div>
    <label>
      Search query (leave empty to show all non-deprecated packages):
      <input name="searchQuery=%s" value="$searchQuery$">
    </label>
  </div>
  <div>
    <label>Page number (zero-indexed):
      <input type=number name="page=%n" min=0 value="$pageNumber$">
    </label>
  </div>
  <input type=submit>
</form>
