

  function loadplot(){
    var layer = "whea_h";

    //request plot using OpenCPU library
    var req = $("#plotdiv").rplot("plotwrapper", {
      var : layer,
      width : 640,
      height : 640
    }).fail(function(){
      alert("Failed to plot stock: " + req.responseText)
    });
  }


  //init
  loadtree();

