<!DOCTYPE html>
<html>
<head>
  $hackageCssTheme()$
  <script src="https://code.jquery.com/jquery-1.12.4.min.js"></script>
  <script src="https://cdn.datatables.net/1.10.12/js/jquery.dataTables.min.js"></script>
  <link rel="stylesheet" href="https://cdn.datatables.net/1.10.12/css/jquery.dataTables.min.css">
  <title>All packages by name | Hackage</title>
</head>

<body>
  $hackagePageHeader()$

  <div id="content">
    <h2>All packages by name</h2>

    <table id ="table">
      <thead>
        <tr><th>Name</th><th>Downloads</th><th>Votes</th><th>Description</th><th>Tags</th><th>Maintainer</th></tr>
      </thead>
      <tbody>
        $tabledata$
      </tbody>
    </table>

  </div>
  <script>
    function filterGlobal () {
      \$('#table').DataTable().search(
        true, true
        ).draw();
    }


    \$(document).ready(function() {
      \$('#table').DataTable();

      \$('input.global_filter').on( 'keyup click', function () {
        filterGlobal();
      } );

    } );
  </script>
</body></html>
