//base OpenCPU API url
//ocpu.seturl("//harvestchoice.ocpu.io/hcapi3/R")

//call R function: stocks::smoothplot(ticker=ticker)
$("#submitbutton").click(function(){
    var layer = $("#layer").val();
    var req = $("#plotdiv").rplot("genPlot", {
        var : layer
    });

    //optional
    req.fail(function(){
        alert("R returned an error: " + req.responseText);
    });
});

