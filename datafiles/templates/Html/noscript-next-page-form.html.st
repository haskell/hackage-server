Showing $minIdx$ to $maxIdx$ of $total$ entries.
$if(hasNext)$
<form method='POST' enctype='multipart/form-data'>
  <input type=hidden name='_method' value='POST'>
  <input type=hidden name='_transform' value='form2json'>
  <input type=hidden name='page=%n' value='$page$'>
  <input type=hidden name='searchQuery=%s' value='$searchQuery$'>
  <input type=hidden name='sortDirection=%s' value='$sortDirection$'>
  <input type=hidden name='sortColumn=%s' value='$sortColumn$'>
  <input type=submit value='Next page (page $page$)'>
</form>
$else$
<p>There are no more pages of results for this query.
$endif$
