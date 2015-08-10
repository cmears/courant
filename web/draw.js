var canvas;

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
    req.open("get", "http://localhost:3000/getruns", true);
    req.send();
}

window.onload = function() {
    fetchruns(function(runs) {
        var div = d3.select(document.body).append("div");
        div.selectAll("div")
          .data(runs)
          .enter()
          .append("div")
          .text(function(d) { return d; })
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
    req.open("get", "http://localhost:3000/getrun/"+uuid, true);
    req.send();
}

function showrun2(run) {
    // canvas = document.getElementById("canvas");
    // var ctx = canvas.getContext('2d');
    // ctx.fillRect(10,10,20,20);
    var mapdiv = d3.select("#map");
    mapdiv.selectAll("svg").remove();
    var svg = mapdiv.append("svg");
    svg.attr("width", 350)
       .attr("height", 350);

    var minTime;
    var maxTime;
    var minLat;
    var maxLat;
    var minLon;
    var maxLon;

    for (var segi in run.segments) {
        for (var sami in run.segments[segi].samples) {
            var sam = run.segments[segi].samples[sami];
            minTime = idundef(Math.min, minTime, sam.time);
            maxTime = idundef(Math.max, maxTime, sam.time);
            minLat = idundef(Math.min, minLat, sam.latitude);
            maxLat = idundef(Math.max, maxLat, sam.latitude);
            minLon = idundef(Math.min, minLon, sam.longitude);
            maxLon = idundef(Math.max, maxLon, sam.longitude);
        }
    }

    console.log(minTime);
    console.log(maxTime);
    console.log(minLat);
    console.log(maxLat);
    console.log(minLon);
    console.log(maxLon);

    // Multiply lat/lon by this to get pixels.
    var scaleFactor = 300 /
          Math.max(maxLat - minLat, maxLon - minLon);

    function pointToPixel(lat, lon) {
        var y = (maxLat - lat) * scaleFactor;
        var x = (lon - minLon) * scaleFactor;
        return { x:x, y:y };
    }

    function drawAtTime(t) {
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
        // while (sami < seg.samples.length && seg.samples[sami].time <= t)
        //     sami++;
        // sami--;

        ctx.clearRect(0,0,350,350);
        var tailLength = 255;
        var i = Math.max(0, sami-tailLength+1);
        while (i <= sami) {
            var sam = seg.samples[i];
            var pixel = pointToPixel(sam.latitude, sam.longitude);
            var size = 5-((sami-i)/(tailLength/5));
            var speed = sam.speed;
            if (i == sami) console.log(speed);
            var r = Math.round(255 - (speed * 50));
            var g = Math.round(speed * 50);
            var b = 0;
            ctx.fillStyle = "rgb(" + r + "," + g + "," + b + ")";
            ctx.fillRect(pixel.x,pixel.y,size,size);
            i++;
        }
    }

    function drawAtTime2(t) {
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

        var tailLength = 10;
        var i = Math.max(0, sami-tailLength+1);
        var idxs = d3.range(i, sami);

        var rects =
          svg.selectAll("rect")
             .data(idxs);

        // console.log(idxs);

        rects
          .enter()
          .append("rect")
          // .attr("x", function(d) { return (seg.samples[d].longitude - minLon)*scaleFactor; })
          // .attr("y", function(d) { return (maxLat - seg.samples[d].latitude)*scaleFactor; })
          .attr("width", 10)
          .attr("height", 10);

        rects
          // .transition()
          // .duration(30)
          .attr("x", function(d) { return (seg.samples[d].longitude - minLon)*scaleFactor; })
          .attr("y", function(d) { return (maxLat - seg.samples[d].latitude)*scaleFactor; })
          .attr("fill", function(d) { return colorForSpeed(seg.samples[d].speed); });
    }

    callWithIncrement(drawAtTime2, minTime, maxTime, 5000, 30);


    // var midTime = (minTime + maxTime)/2;
    // drawAtTime(midTime);

    // for (var segi in run.segments) {
    //     for (var sami in run.segments[segi].samples) {
    //         var sam = run.segments[segi].samples[sami];
    //         var pixel = pointToPixel(sam.latitude, sam.longitude);
    //         ctx.fillRect(pixel.x,pixel.y,5,5);
    //     }
    // }
}


function callWithIncrement(f, start, end, inc, delay) {
    if (start > end) return;
    f(start);
    setTimeout(function(){callWithIncrement(f, start+inc, end, inc, delay);},
               delay);
}

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

function colorForSpeed(s) {
    var r = Math.round(255 - (s * 12));
    var g = Math.round(s * 12);
    var b = 0;
    return "rgb(" + r + "," + g + "," + b + ")";
}
