<!DOCTYPE html>
<html>

    <head>
        $hackageCssTheme()$
        <script src="/static/jquery.min.js"></script>
        <script src="/static/datatables/jquery.dataTables.min.js"></script>
        <link rel="stylesheet" href="/static/datatables/jquery.dataTables.min.css">
        <title>All packages by name | Hackage</title>
    </head>

    <body>
        $hackagePageHeader()$

        <div id="content">
            <h1>$heading$</h1>
            $content$
            <table id="table" style="min-width:650px;width:100%" class="fancy row-border order-column compact">
                <thead>
                    <tr>
                        <th><div style="width:100px">Name</div></th>
                        <th><div style="width:50px"><span title="(30 days)">DLs</span></div></th>
                        <th><div style="width:50px"><span title="(0-3)">Rating</span></div></th>
                        <th><div style="width:50px">Rev Deps</div></th>
                        <th><div style="min-width:160px">Description</div></th>
			<th><div style="width:140px">Tags</div></th>
			<th><div style="width:80px">Last U/L</th>
                        <th><div style="width:100px">Maintainer</div></th>
                    </tr>
                </thead>
                <tbody>
                    $tabledata$
                </tbody>
            </table>
            $footer$
        </div>
        <script>
            function filterGlobal() {
                \$('#table').DataTable().search(
                    true, true
                ).draw();
            }

            \$(document).ready(function() {

                \$.extend( jQuery.fn.dataTableExt.oSort, {
                   "package-pre": function ( a ) {
                        return [a.search(/deprecated/),a.toLowerCase()];
                    },
                    "package-asc": function (a, b) {
                        return ((a < b) ? -1 : ((a > b) ? 1 : 0));
                    },
                    "package-desc": function (a, b) {
                        return ((a < b) ? 1 : ((a > b) ?  -1 : 0));
                    }
                } );

                \$('#table').removeAttr('width').DataTable({
                    "order": [],
                    "bProcessing": true,
                    "bDeferRender": true,
                    "autoWidth": false,
		    "pageLength":50,
                    "language": {
                          "search": "Filter:"
                     },
                    "columnDefs": [
			{ type: "package", "targets": [0] },
                        { className: "dt-body-right", "targets": [1, 2, 3 ] }
                     ]
                });

                \$('input.global_filter').on('keyup click', function() {
                    filterGlobal();
                });
            });

        </script>
    </body>

</html>
