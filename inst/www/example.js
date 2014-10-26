//base OpenCPU API url
//ocpu.seturl("//harvestchoice.ocpu.io/hcapi3/R")

//call R function: stocks::smoothplot(ticker=ticker)
$("#submitbutton").click(function () {
    var layer = $("#layer").val();
    var req = $("#plotdiv").rplot("genPlot", {
        "var": layer
    });

    //optional
    req.fail(function () {
        alert("R returned an error: " + req.responseText);
    });
});

//call R function: tvscore::tv(input=data)
$("#aggbutton").click(function () {
    var layer = $("#layer").val();
    var by = $("#by").val();

    var req = ocpu.rpc("getLayer", {
        "var": layer,
        "by": by
    }, function (output) {
        $("tbody").empty();
        $.each(output, function (index, value) {
            var html = "<tr><td>" + value.ISO3 + "</td><td>" + value.whea_p + "</td></tr>";
            $("tbody").append(html);
        });
    });

