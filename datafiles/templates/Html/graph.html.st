<!DOCTYPE html>
<html>

    <head>
        <title>Hackage Dependencies Graph</title>
        <script src="/static/graph/vivagraph.js"></script>
        <script src="/static/jquery.min.js"></script>`
        <script src="/static/graph/tmpl.js"></script>

        <link rel="stylesheet" href="/static/graph/graph.css">
    </head>

    <body>
        <div class="graph-full-size" id="graph">
            <div class="container row">
                <div class="search col-xs-12 col-sm-6 col-md-4">
                    <form class="search-form" role="search">
                        <div class="input-group">
                            <input class="form-control no-shadow" type="text" placeholder="enter a search term">
                            <span class="input-group-btn">
                                <button class="btn" id="go" tabindex="-1">Go</button>
                                <button onclick="clearSuggestion();" class="btn" tabindex="-1">Clear</button>
                            </span>
                        </div>
                    </form>
                </div>
            </div>
            <div class="navigation-help">
                <div class="label about"></div>
            </div>
        </div>
        <div id="result"></div>
        <div id="tooltip"></div>
        <div id="log"></div>
    </body>
    <script type='text/javascript'>

        \$.getJSON('/packages/graph.json', onLoad)

        function onLoad(json) {
            var graphGenerator = Viva.Graph.generator();
            var graph = Viva.Graph.graph();

            // generate nodes and links
            for (var i = 0; i < json.length; i++) {
                var node = json[i]
                graph.addNode(node.name, node.id);
                for (var j = node.deps.length - 1; j >= 0; j--)
                    graph.addLink(node.name, node.deps[j])
            }

            var layout = Viva.Graph.Layout.forceDirected(graph, {
                springLength: 150,
                springCoeff: 0.0001,
                dragCoeff: 0.002,
                gravity: -1.2
            });

            // for faster results
            precompute(200, renderGraph);

            function precompute(iterations, callback) {
                // 10 iterations per event loop cycle:
                var i = 0;
                while (iterations-- > 0 && i++ < 10) layout.step();
                if (iterations > 0)
                    setTimeout(function() {
                        precompute(iterations, callback);
                    }, 0);
                else callback();
            }

            function renderGraph() {
                var graphicsOptions = {
                    clearColor: true,
                    clearColorValue: { // use black color to erase background
                        r: 0,
                        g: 0,
                        b: 0,
                        a: 1
                    }
                };
                var graphics = Viva.Graph.View.webglGraphics(graphicsOptions);

                var renderer = Viva.Graph.View.renderer(graph, {
                    layout: layout,
                    container: document.getElementById('graph'),
                    graphics: graphics,
                    renderLinks: true,
                    prerender: true
                });

                // fix node positions
                graph.forEachNode(function(node) {
                    layout.pinNode(node, true);
                });

                var events = Viva.Graph.webglInputEvents(graphics, graph);

                events.mouseEnter(function(node) {

                    var count = nodeCount(node);
                    var ui = graphics.getNodeUI(node.id);

                    var pos = ui.position;
                    graphics.transformGraphToClientCoordinates(pos);
                    var data = {
                        "top": pos.y + 'px',
                        "left": pos.x + 'px',
                        "name": node.id,
                        "dependencies": count[1],
                        "dependents": count[0]
                    };
                    document.getElementById("tooltip").innerHTML = tmpl("tooltip-tmpl", data);

                    highlightNode(node.id);
                })
                .mouseLeave(function(node) {
                    document.getElementById("tooltip").innerHTML = '';
                }).dblClick(function(node) {

                }).click(function(node) {

                    var count = nodeCount(node);
                    var data = {
                        "package": node.id,
                        "dependencies": count[1],
                        "dependents": count[0]
                    };
                    document.getElementById("result").innerHTML = tmpl("package-tmpl", data);
                });


                renderer.run();

                \$('.input-group input').on('keyup', function() {
                    var val = \$.trim(this.value);
                    var resultSet = \$.grep(json, function(e) {
                        return (e.name.indexOf(val) != -1);
                    });
                    var data = {
                        "packages": [],
                        "count": resultSet.length
                    };
                    if (val.length > 0) {
                        \$.each(resultSet, function(i) {
                            var nodeInfo = graph.getNode(resultSet[i].name);
                            // console.dir(nodeInfo)
                            count = nodeCount(nodeInfo);
                            data.packages.push({
                                "name": nodeInfo.id,
                                "dependents": count[0],
                                "dependencies": count[1]
                            })

                        });
                        document.getElementById("log").innerHTML = tmpl("search-tmpl", data);
                    }
                });

                function clearSuggestion() {
                    alert("lol");
                    document.getElementById("log").innerHTML = '';

                }

                \$('.search-form').submit(function(e) {
                    document.getElementById("log").innerHTML = '';
                    e.preventDefault();
                    var nodeId = \$('.input-group input').val();
                    if (graph.getNode(nodeId)) {
                        var pos = layout.getNodePosition(nodeId);
                        renderer.reset()
                        renderer.moveTo(pos.x, pos.y);
                        highlightNode(nodeId);
                    }
                });

                var prevSelected;
                var prevSelectedLinks;

                function highlightNode(nodeId) {
                    var ui = graphics.getNodeUI(nodeId);
                    var nodeInfo = graph.getNode(nodeId);

                    if (prevSelected) {
                        prevSelected.color = 0x00a2e8ff;
                        prevSelected.size = 10;
                    }

                    prevSelected = ui;
                    if (prevSelectedLinks) {
                        prevSelectedLinks.forEach(function(link) {
                            var lol = graphics.getNodeUI(link[0].id);
                            var loll = graphics.getLinkUI(link[1].id);
                            lol.color = 0x00a2e8ff;
                            loll.color = 0xb3b3b3ff
                        });
                    }

                    prevSelectedLinks = [];
                    graph.forEachLinkedNode(nodeId, function(linkedNode, link) {
                        var lol = graphics.getNodeUI(linkedNode.id);
                        var loll = graphics.getLinkUI(link.id);
                        prevSelectedLinks.push([linkedNode, link])
                        lol.color = 0xFFFFFFff;
                        loll.color = 0xFFA500ff
                    });
                    ui.color = 0xFFA500ff;
                    ui.size = 20;
                }

                var graphRect = layout.getGraphRect();
                var graphSize = Math.min(graphRect.x2 - graphRect.x1, graphRect.y2 - graphRect.y1);
                var screenSize = Math.min(window.innerHeight, window.innerWidth);
                var desiredScale = screenSize / graphSize;
                zoomOut(desiredScale, 1);

                function zoomOut(desiredScale, currentScale) {
                    if (desiredScale < currentScale) {
                        currentScale = renderer.zoomOut();
                        setTimeout(function() {
                            zoomOut(desiredScale, currentScale);
                        }, 1);
                    }
                }
            }

            // Utility function to return o of dependencies and dependents
            function nodeCount(node) {
                var dependents = 0,
                    dependencies = 0;
                for (var i = node.links.length - 1; i >= 0; i--) {
                    node.links[i].fromId == node.id ? dependencies++ : dependents++;
                }
                return [dependents, dependencies];
            }

        }

    </script>

    <!-- tmpl.js templates for description on click, search results and tooltip -->
    <script type="text/x-tmpl" id="package-tmpl">
        <div class="node-details">
            <div class="container-fluid row">
                <div class="hidden-xs">
                    <div class="col-xs-6"><a target="_blank" href="/package/{%=o.package%}"><h4  title="{%=o.package%}">{%=o.package%}</h4></a></div>
                    <div class="col-xs-3">
                        <div class="row">
                            <h2 class="in-degree" id="258998">{%=o.dependents%}</h2></div>
                        <div class="row small">dependents</div>
                    </div>
                    <div class="col-xs-3">
                        <div class="row">
                            <h2 class="out-degree" id="258998">{%=o.dependencies%}</h2></div>
                        <div class="row small">dependency</div>
                    </div>
                </div>
                <div class="visible-xs-block">
                    <div class="row info-block">
                        <div class="col-xs-6 no-overflow"><a target="_blank" href="/package/{%=o.package%}">{%=o.package%}</a></div>
                        <div class="in-degree col-xs-3" id="258998">{%=o.dependents%}</div>
                        <div class="out-degree col-xs-3" id="258998">{%=o.dependencies%}</div>
                    </div>
                </div>
            </div>
        </div>
    </script>

    <script type="text/x-tmpl" id="search-tmpl">
        <div class="window-container search-results-window">
            <h4 class="window-title"><span >Found </span><strong >{%=o.count %}</strong><span > matches</span></h4>
            <div class="window-list-content">
                <div style="position:relative;height:43400px;">
                    <div style="-ms-transform:translate(0px, 0px);-webkit-transform:translate(0px, 0px);transform:translate(0px, 0px);">
                        <div>
                            {% for (var i=0; i
                            <o.packages.length; i++) { %} <div class="row">
                                <div class="no-oveflow col-md-6 col-xs-6">
                                    <a target="_blank" href="/package/{%=o.packages[i].name%}"><span class="node-focus" id="64">{%=o.packages[i].name %}</span></a>

                                </div>
                                <div class="in-degree col-md-3 col-xs-3" id="64">{%=o.packages[i].dependents %}</div>
                                <div class="out-degree col-md-3 col-xs-3" id="64">{%=o.packages[i].dependencies %}</div>
                        </div>
                        {% } %}
                    </div>
                </div>
            </div>
        </div>
        </div>
    </script>

    <script type="text/x-tmpl" id="tooltip-tmpl">
        <div class="node-hover-tooltip" style="left: {%=o.left %}; top: {%=o.top %};"><span>{%=o.name %}</span><span class="in-degree">{%=o.dependents %}</span><span class="out-degree">{%=o.dependencies %}</span></div>
    </script>

</html>
