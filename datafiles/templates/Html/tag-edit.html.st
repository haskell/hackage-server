<!DOCTYPE html>
<html>

    <head>
        $hackageCssTheme()$
        <title>Edit package tags | Hackage</title>
        <script src="/static/jquery.min.js"></script>
        <style>
            .accept {
                color: green;
            }
            .reject {
                color: red;
            }
            .eadd,
            .edel {
                clear: both;
            }
            #additions,
            #deletions {
                list-style: none;
                margin: 0px;
            }
            #additions li,
            #deletions li {
                padding: 1%;
                margin: 1%;
                float: left;
                display: inline-block;
                border: 1px solid black;
            }
            #additions:hover,
            #deletions:hover {
                background: white;
            }
        </style>
    </head>

    <body>
        $hackagePageHeader()$
        <div id="content">
            Set tags for <a href="/package/$pkgname$">$pkgname$</a>
            <div class="box">
                <p><b>Current Tags</b>
                    <br />$tags$</p>
                <form method="post" action="/package/$pkgname$/tags">
                    <input type="hidden" name="_method" id="_method" value="PUT" />
                    <dl>
                        <dd>
                            <label for="addns">Propose Additions </label>
                            <input type="text" value="" name="addns" id="addns" />
                        </dd>
                    </dl>
                    <dl>
                        <dd>
                            <label for="delns">Propose Deletions </label>
                            <input type="text" value="" name="delns" id="delns" />
                        </dd>
                    </dl>
                    <input hidden="true" type="text" value="" name="raddns" id="raddns" />
                    <input hidden="true" type="text" value="" name="rdelns" id="rdelns" />
                    <input type="submit" value="Propose tags" />
                </form>
                $if(isuser)$
                <div class="proposals">
                    <big>Other proposals</big>
                    <span class="eadd"><br><b>additions: </b>$addns$</span>
                    <span class="edel"><br><b>deletions: </b>$delns$</span>
                </div>
                $endif$ $if(istrustee)$
                <div id="trustee" class="proposals">
                    <big>Proposals</big>
                    <span class="eadd"><br><b>additions: </b>
                  <ul id ="additions"></ul>
              </span>
                    <div style="clear:both"> </div>
                    <span class="edel"><br><b>deletions: </b>
                  <ul id = "deletions"></ul>
              </span>
                </div>
                <script>
                    var insert = function(element, tag) {
                        if (element.val().trim())
                            element.val(element.val() + ',' + tag)
                        else
                            element.val(tag)
                    }

                    \$("#trustee").on("click", "li span", function(e) {
                        \$(this).parent().hide()
                        var tag = \$(this).parent().text().trim().split(' ')[0]
                        if (\$(this).attr('class') == 'accept') {
                            if (\$(this).parent().parent().attr('id') == 'additions')
                                insert(\$("#addns"), tag)
                            else if (\$(this).parent().parent().attr('id') == 'deletions')
                                insert(\$("#delns"), tag)
                        } else if (\$(this).attr('class') == 'reject') {
                            if (\$(this).parent().parent().attr('id') == 'additions')
                                insert(\$("#raddns"), tag)
                            else if (\$(this).parent().parent().attr('id') == 'deletions')
                                insert(\$("#rdelns"), tag)
                        }
                    })
                    if ("$delns$".trim() == "" && "$addns$".trim() == "")\$('.proposals').each(function(i, obj) {
                        \$(this).hide()
                    });
                    var aList = \$('#additions')
                    var dList = \$("#deletions")

                    if ("$addns$".trim() != "") {
                        var ads = "$addns$".replace(/\s/g, '').split(',')
                        \$.each(ads, function(i) {
                            var li = \$('<li/>')
                                .text(ads[i])
                                .appendTo(aList);
                            var aaa = \$("<span class='accept'> &#x2713;</span> <span class='reject'>&#x2717;</span>")
                                .appendTo(li);
                        });
                    }


                    if ("$delns$".trim() != "") {
                        var dels = "$delns$".replace(/\s/g, '').split(',')

                        \$.each(dels, function(i) {
                            var li = \$('<li/>')
                                .text(dels[i])
                                .appendTo(dList);
                            var aaa = \$("<span class='accept'> &#x2713;</span> <span class='reject'>&#x2717;<span>")
                                .appendTo(li);
                        });
                    }

                </script>
                $endif$
                <div style="clear:both"> </div>
            </div>
        </div>
        $footer()$
    </body>
    <script>
        if ("$addns$".trim() == "" || "$delns$".trim() == "") {
            if ("$addns$".trim() != "")
                \$(".edel").each(function(i, obj) {
                    \$(this).hide()
                });
            else if ("$delns$".trim() != "")
                \$(".eadd").each(function(i, obj) {
                    \$(this).hide()
                });
            else
                \$(".proposals").each(function(i, obj) {
                    \$(this).hide()
                });
        }
    </script>
</html>
