<!DOCTYPE html>
<html>

    <head>
        $hackageCssTheme()$
        <script src="/static/jquery.min.js"></script>
        <script src="/static/datatables/jquery.dataTables.min.js"></script>
        <link rel="stylesheet" href="/static/datatables/jquery.dataTables.min.css">
        <title>Package candidates | Hackage</title>
    </head>

<body>
  $hackagePageHeader()$
  <div id="content">
            <h1>$heading$</h1>
            $content$
            $list$
            <!-- <table id="table" style="min-width:650px;width:100%" class="fancy row-border order-column compact">
                <thead>
                    <tr>
                        <th><div style="width:100px">Name</div></th>
                        <th><div style="width:50px"><span title="(30 days)">DLs</span></div></th>
                        <th><div style="width:50px"><span title="(0-3)">Rating</span></div></th>
                        <th><div style="min-width:160px">Description</div></th>
                        <th><div style="width:140px">Tags</div></th>
                        <th><div style="width:80px">Last U/L</th>
                        <th><div style="width:100px">Maintainer</div></th>
                    </tr>
                </thead>
                <tbody>
                    $tabledata$
                </tbody>
            </table> -->
            $footer$
        </div>
</body>
</html>