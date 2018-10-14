function htmlWidgetsHook(el, width, height, data) {
    window.data = data;
    window.el = el;
    drawBubblePlot(el, width, height, data.data, data.top, true);
}

function htmlResizeHook(width, height) {
    updateBubblePlot(width, height);
}

function updateBubblePlot(width, height) {
    d3.select(el)
        .select("div")
        .remove();

    drawBubblePlot(window.el, width, height, window.data.data, window.data.top, false);
}

function drawBubblePlot(el, width, height, data, top, reformat_data) {
    // data needs to be reformatted for bubble structure
    if (reformat_data) {
        data = get_unique(data);
        data = data.sort(function(a,b) { return b.downloads_month - a.downloads_month; });
        // convert numerical values from strings to numbers
        data = data.map(function(d) { d.value = Math.sqrt(+d.downloads_total); return d; });
        // split tags
        data = data.map(function(d) { d.tags = typeof d.tags === "undefined" ? "" : d.tags.split(","); return d; });        
    }
    var diameter = Math.min(width, height), // max size of the bubbles
        color    = d3.scale.category20c(); // color category

    var bubble = d3.layout.pack()
        .sort(function(a,b) { return b.downloads_month - a.downloads_month; })
        .size([diameter, diameter])
        .padding(1.5);

    var svg = d3.select(el)
        .append("div")
        .style("width", diameter+"px")
        .style("height", diameter+"px")
        .append("svg")
        .attr("width", diameter)
        .attr("height", diameter)

    var allTags = _.flatten(data.map(function(d) { return d.tags; }));
    var tagCount = _.countBy(allTags, function(d) { return d; });
    allTags = _.uniq(allTags).filter(function(d) { return tagCount[d] > 10; });

    var partialData = topFilter(data, top);

    function drawChart(data) {
        // bubbles needs very specific format, convert data to this.
        var nodes = bubble.nodes({children:data}).filter(function(d) { return !d.children; });

        // setup the chart
        var container = d3.select("svg").select("g");

        if (container.empty()) {
            container = svg.append("g").attr("transform", "translate(0,0)");

            drawOptions();
            drawBackground();
            drawInfoBox();
        }

        // redraw existing bubbles
        var bubbleSelect = container.selectAll(".bubble").data(nodes, function(d) { return d.name; });
        bubbleSelect.attr("class", "node bubble")
                    .transition(250)
                    .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; })
                    .selectAll("circle")
                    .attr("r", function(d) { return d.r; });

        // resize existing text
        bubbleSelect.selectAll("text")
                    .text(function(d) { return d.name.substring(0, d.r/4); });

        // draw new bubbles
        var bubbles = bubbleSelect.enter()
                                .append("g")
                                .attr("class", "node bubble")
                                .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });

        // remove deleted bubbles
        bubbleSelect.exit().remove();

        // create mouseover text
        bubbles.append("title")
                .text(function(d) { return `${d.name}: ${d.downloads_total}`; });

        // create the bubbles
        bubbles.append("circle")
                .attr("r", function(d) { return 0; })
                .on("click", function(d) { clickFunc(d); })
                .style("fill", function(d) { return color(d.name); })
                .transition()
                .delay(function(d, i) { return Math.floor(20*i/data.length)*50; })
                .attr("r", function(d) { return d.r; });

        // format the text for each bubble
        bubbles.append("text")
                .attr("class", "bubble-text")
                .attr("dy", ".3em")
                .style("text-anchor", "middle")
                .style("fill", "white")
                .text(function(d) { return d.name.substring(0, d.r/4); });
    }

    drawChart(partialData);

    $(document).keydown(
		function(e) {
			// esc escapes out of info screen
			if (e.keyCode === 27) curtainClick();
		}
	);

    function clickFunc(d) {
        curtainUp();
        infoBoxUp();
        d3.select("div.info-box")
            .style("border-color", color(d.name));
        d3.select("div.package-name")
            .style("background", color(d.name));
        populateInfoBox(d.name,
                        d.downloads_month,
                        d.description,
                        d.license,
                        d.authors,
                        d.page);
    }
    
    function curtainUp() {
        d3.select("div.bg")
            .style({"opacity": 0.5,
                    "cursor": "pointer",
                    "pointer-events": "all"});
    }
    
    function curtainDown() {
        d3.select("div.bg")
            .style({"opacity": 0.0,
                    "cursor": "default",
                    "pointer-events": "none"});
    }
    
    function curtainClick() {
        curtainDown();
        infoBoxDown();
    }
    
    function infoBoxUp() {
        d3.select("div.info-box")
            .style({"opacity": 1,
                    "pointer-events": "all"});
    }
    
    function infoBoxDown() {
        d3.select("div.info-box")
            .style({"opacity": 0,
                    "pointer-events": "none"});
    }
    
    function infoBoxClick() {
        console.log("Info box clicked!");
    }
    
    function drawOptions() {
        var outer = d3.select("body")
                        .append("div")
                        .attr("class", "dropdown")
                        .style("position", "absolute")
                        .style("top", "10px")
                        .style("left", "10px");
    
        outer.append("button")
                .attr("class", "btn btn-primary dropdown-toggle")
                .attr("type", "button")
                .attr("data-toggle", "dropdown")
                .html("BiocViews Filter")
                .append("span")
                .attr("class", "caret");
    
    
        var inner = outer.append("ul").attr("class", "dropdown-menu scrollable-menu");
    
        for (var i = 0; i < allTags.sort().length; i++) {
            inner.append("li")
                .append("a")
                .attr("href", "#")
                .html(allTags[i])
                .on("click", function(d) { 
                    d3.select(this.parentNode.parentNode)
                        .selectAll("li")
                        .classed("disabled", false);
                    d3.select(this.parentNode).classed("disabled", true);
                    filterByTag(this.innerHTML);
                });
        }
    }
    
    function drawBackground() {
        d3.select("body")
            .append("div")
            .attr("class", "bg")
            .style({"position": "absolute",
                    "top": "0",
                    "left": "0",
                    "right": "0",
                    "bottom": "0",
                    "height": "100%",
                    "width": "100%",
                    "z-index": "99",
                    "background": "#666666",
                    "opacity": 0.0,
                    "pointer-events": "none"})
            .on("click", curtainClick);
    }
    
    function drawInfoBox() {
        // remove old info box
        var sel = d3.select("div.info-box");
        if (sel.node() == null) {
            d3.select("body")
                .append("div")
                .attr("class", "info-box")
                .style({"position": "absolute",
                "width": "400px",
                "z-index": "100",
                "background": "#ffffff",
                "opacity": 0.0,
                "pointer-events": "none"})
                .on("click", infoBoxClick);

            layoutInfoBox();
        }

        
    }
    
    function layoutInfoBox() {
        d3.select("div.info-box")
            .append("div")
            .attr("class", "package-name row");
    
        newRow("description");
        newRow("downloads");
        newRow("license");
        newRow("authors");
        newRow("install");
        newRow("page");
    
        d3.select("div.info-box")
            .select(".row.page")
            .select("p")
            .append("a");
    
        function newRow(divName) {
            row = d3.select("div.info-box")
                    .append("div")
                    .attr("class", divName + " info-text row");
    
            row.append("div")
                .attr("class", "info-title");
            row.append("p");
        }
    }
    
    function populateInfoBox(name, downloads_month, description, license, authors, page) {
        d3.select("div.info-box")
            .select("div.package-name")
            .html(name);
    
        fillSection("description", "Description", description);
        fillSection("downloads", "Downloads Last Month", downloads_month);
        fillSection("license", "License", license);
        fillSection("authors", "Authors", authors);
        fillSection("install", "Install", "source(\"https:// bioconductor.org/biocLite.R\")<br />biocLite(\"" + name + "\")");
    
        d3.select("div.info-box")
            .select(".row.page")
            .select("div.info-title")
            .html("Home Page");
    
        d3.select("div.info-box")
            .select(".row.page")
            .select("p")
            .select("a")
            .attr("href", page)
            .html(page);
    
        function fillSection(section, title, text) {
            d3.select("div.info-box")
                .select(".row." + section)
                .select("div.info-title")
                .html(title);
    
            d3.select("div.info-box")
                .select(".row." + section)
                .select("p")
                .html(text);
        }
    }
    
    function get_unique(data) {
        keys = data.map(function(d) { return d.name; });
        seen = {};
        for (var i = 0; i  < keys.length; i++) {
            seen[keys[i]] = false;
        }
        new_data = [];
        for (i = 0; i < data.length; i++) {
            if (!seen[data[i].name]) {
                new_data.push(data[i]);
                seen[data[i].name] = true;
            }
        }
    
        return new_data;
    }
    
    function topFilter(data, n) {
        return data.slice(0, n);
    }
    
    function tagFilter(data, tag) {
        return data.filter(function(d) { return _.contains(d.tags, tag); });
    }
    
    function filterByTag(tag) {
        partialData = tagFilter(data, tag);
        partialData = topFilter(partialData, top)
        drawChart(partialData);
    }
}
