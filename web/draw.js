var canvas;

var hostname = "courant.cmears.id.au";
hostname = "localhost:3000";

function idundef(f,x,y) {
    if (x === undefined) return y;
    if (y === undefined) return x;
    return f(x,y);
}

function fetchruns(callback) {
    var req = new XMLHttpRequest();
    req.addEventListener('load', function() {
        var runList = JSON.parse(this.responseText);
        callback(runList);
    });
    req.open("get", "http://"+hostname+"/getruns", true);
    req.send();
}

window.onload = function() {
    fetchruns(function(runs) {
        var div = d3.select(document.body).append("div");
        div.selectAll("div")
          .data(runs)
          .enter()
          .append("div")
          .text(function(d) {
              return d;
          })
          .on("click", function(d) {
              showrun(d);
          });
        d3.select(document.body).append("div")
          .attr("id", "map")
          .attr("width", 350)
          .attr("height", 350);
    });
}

function showrun(uuid) {
    var req = new XMLHttpRequest();
    req.addEventListener('load', function() {
        var run = JSON.parse(this.responseText);
        showrun2(run);
    });
    req.open("get", "http://"+hostname+"/getrun/"+uuid, true);
    req.send();
}

function runExtremities(run) {
    var minTime;
    var maxTime;
    var minLat;
    var maxLat;
    var minLon;
    var maxLon;
    var minSpeed;
    var maxSpeed;

    for (var segi in run.segments) {
        for (var sami in run.segments[segi].samples) {
            var sam = run.segments[segi].samples[sami];
            minTime = idundef(Math.min, minTime, sam.time);
            maxTime = idundef(Math.max, maxTime, sam.time);
            minLat = idundef(Math.min, minLat, sam.latitude);
            maxLat = idundef(Math.max, maxLat, sam.latitude);
            minLon = idundef(Math.min, minLon, sam.longitude);
            maxLon = idundef(Math.max, maxLon, sam.longitude);
            minSpeed = idundef(Math.min, minSpeed, sam.speed);
            maxSpeed = idundef(Math.max, maxSpeed, sam.speed);
        }
    }

    return { minTime: minTime
           , maxTime: maxTime
           , minLat: minLat
           , maxLat: maxLat
           , minLon: minLon
           , maxLon: maxLon
           , minSpeed: minSpeed
           , maxSpeed: maxSpeed
           };
}

function showrun2(run) {
    var mapdiv = d3.select("#map");
    mapdiv.selectAll("*").remove();

    var info = mapdiv.append("div");
    var totalDistance = 0;
    for (var segi in run.segments) {
        for (var sami in run.segments[segi].samples) {
            var sampleDistance = run.segments[segi].samples[sami].distance;
            if (sampleDistance !== undefined)
                totalDistance += sampleDistance;
            run.segments[segi].samples[sami].cumulativeDistance = totalDistance;
        }
    }
    run.totalDistance = totalDistance;

    var runStartTime = run.segments[0].samples[0].time;
    var runEndTime = (run.segments.slice(-1)[0]).samples.slice(-1)[0].time;
    var runDuration = runEndTime-runStartTime;
    var runStartDate = new Date(runStartTime);

    info.html("distance: " + Math.trunc(run.totalDistance) + "m" + "<br/>"
              + "start time: " + runStartDate + "<br/>"
              + "duration: " + showDuration(Math.round(runDuration/1000)) );

    var map = mapdiv.append("svg");
    map.attr("width", 350)
       .attr("height", 350);
    var mapObj = makeMap(map, run);

    var chart = mapdiv.append("svg");
    chart.attr({width:350, height:350});
    var chartObj = makeChart(chart, run);

    chart.on("mousemove", function() {
        var mouseCoord = d3.mouse(this);
        var t = chartObj.xScale.invert(mouseCoord[0]);
        chartObj.highlightTime(t);
        mapObj.highlightTime(t);
    });

    var exts = runExtremities(run);

    chart.on("click", function() {
        var f = function(t) {
            chartObj.highlightTime(t);
            mapObj.highlightTime(t);
        };
        var mouseCoord = d3.mouse(this);
        var t = chartObj.xScale.invert(mouseCoord[0]);
        callWithIncrement(f, t, exts.maxTime, 1000, 15);
    });
}


function callWithIncrement(f, start, end, inc, delay) {
    if (start > end) return;
    f(start);
    setTimeout(function(){callWithIncrement(f, start+inc, end, inc, delay);},
               delay);
}

// Returns the largest value for which the leq predicate returns true.
// If the predicate never returns true, returns lower - 1.  Assumes
// ordering: the predicate should be true at the beginning, then
// switch permanently to false, e.g.
//
// lower   lower+1   ...  x-1  x  x+1  ...  upper-1  upper
//   T       T             T   T   F           F       F
function binSearch(leq, lower, upper) {
    if (lower == upper && leq(lower))
        return lower;
    if (lower == upper)
        return lower - 1;
    var m = Math.trunc((lower + upper) / 2);
    if (leq(m)) {
        lower = m + 1;
    } else {
        upper = m;
    }
    return binSearch(leq, lower, upper);
}

function makeMap(svg, run) {
    var exts = runExtremities(run);

    var midLat = (exts.minLat + exts.maxLat) / 2;
    var midLon = (exts.minLon + exts.maxLon) / 2;
    var latRange = exts.maxLat - exts.minLat;
    var lonRange = exts.maxLon - exts.minLon;
    var range = Math.max(latRange, lonRange);
    var minLat = midLat - range/2;
    var maxLat = midLat + range/2;
    var minLon = midLon - range/2;
    var maxLon = midLon + range/2;

    var xScale = d3.scale.linear().domain([minLon, maxLon])
                                  .range([0, svg.attr("width")]);
    var yScale = d3.scale.linear().domain([minLat, maxLat])
                                  .range([svg.attr("height"), 0]);

    var path;
    for (var segi in run.segments) {
        var l = d3.svg.line()
          .x(function(d) { return xScale(d.longitude); })
          .y(function(d) { return yScale(d.latitude); })
          .interpolate("basis");
        var pathSel = svg.append("path")
          .attr("d", l(run.segments[segi].samples))
          .style("stroke", "black")
          .style("fill", "none");
        path = pathSel.node();
    }

    var marker = svg.append("circle")
        .attr("r", 0)
        .style("fill", "red")
        .style("opacity", 0.75);

    var highlightTime = function(t) {
        // Find the sample closest to the given time.
        // First find the segment.
        var segi = 0;
        while (segi < run.segments.length && run.segments[segi].samples[0].time <= t)
            segi++;
        segi--;
        // Now find the sample.
        var seg = run.segments[segi];
        var sami = 0;
        sami = binSearch(function(i) { return seg.samples[i].time <= t; },
                         0, seg.samples.length);

        // Now the time t is between sample sami and the one after it
        // (if it exists).  Interpolate between these two.
        var runDist;
        if (sami == seg.samples.length-1) {
            // At the end; just take this sample.
            runDist = seg.samples[sami].cumulativeDistance;
        } else {
            // Interpolate.
            var prevTime = seg.samples[sami].time;
            var nextTime = seg.samples[sami+1].time;
            var prevDist = seg.samples[sami].cumulativeDistance;
            var nextDist = seg.samples[sami+1].cumulativeDistance;
            var frac = (t - prevTime) / (nextTime - prevTime);
            runDist = prevDist + frac*(nextDist-prevDist);
        }
            
        var runFraction = runDist / run.totalDistance;
        var pathLength = path.getTotalLength();
        var point = path.getPointAtLength(runFraction * pathLength);
        marker.attr("cx", point.x);
        marker.attr("cy", point.y);
        marker.attr("r", 10);
    }

    return { highlightTime: highlightTime };
}

function makeChart(svg, run) {
    var exts = runExtremities(run);
    var xScale = d3.scale.linear().domain([exts.minTime, exts.maxTime])
                                  .range([0, svg.attr("width")]);
    var yScale = d3.scale.linear().domain([0, exts.maxSpeed])
                                  .range([svg.attr("height"), 0]);

    // Draw the horizontal guide lines.
    for (var i = 2 ; i <= exts.maxSpeed ; i += 2) {
        svg.append("line")
          .attr({ x1: 0
                , y1: yScale(i)
                , x2: svg.attr("width")
                , y2: yScale(i)
                })
          .style("stroke", "#cccccc")
    }
    
    // Draw the chart itself.
    for (var segi in run.segments) {
        var l =
          d3.svg.line()
            .x(function(d) { return xScale(d.time); })
            .y(function(d) { return yScale(d.speed); })
            .interpolate("basis");

        svg.append("path")
          .attr("d", l(run.segments[segi].samples))
          .style("stroke", "black")
          .style("fill", "none");
    }

    var highlightBar = svg.append("rect")
                            .attr("width", 0);
    var highlightWidth = 3;

    var highlightTime = function(t) {
        highlightBar.attr({ x: xScale(t) - highlightWidth
                          , y: 0
                          , width: highlightWidth*2
                          , height: svg.attr("height")
                          })
                       .style("fill", "red")
                       .style("opacity", 0.2);
    }

    return { highlightTime: highlightTime
           , xScale: xScale };
}

function showDuration(seconds) {
    var m = Math.trunc(seconds / 60);
    var s = seconds % 60;
    return m + ":" + s
}
