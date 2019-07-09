Experigen.initialize = function () {

	var items  = this.resource("items");
//  var frames = this.resource("frames");

//	items = items.pairWith("frame", frames.shuffle())

	var samples = items.subset("type","trial");

  var sampleItem = []
	 		.concat(samples.chooseFirst(4))
			.pairWith("order",1)
			.pairWith("view","confidence.ejs")
			;

  var nc = items.subset("type","Critical_1").shuffle();
	var dn = items.subset("type","Critical_2").shuffle();
	var allnot = items.subset("type","Filler_1").shuffle();
	var notall = items.subset("type","Filler_2").shuffle();


	var block1 = []
			.concat(nc.chooseFirst(1))
	    .concat(allnot.chooseFirst(1))
	  	.concat(dn.chooseFirst(1))
	    .concat(notall.chooseFirst(1))
			.pairWith("view","stimulus.ejs")
			;

	var block2 = []
			.concat(nc.excludeFirst(1).chooseFirst(1))
			.concat(allnot.excludeFirst(1).chooseFirst(1))
 	   	.concat(dn.excludeFirst(1).chooseFirst(1))
     	.concat(notall.excludeFirst(1).chooseFirst(1))
			.pairWith("view","stimulus.ejs")
			;

	var block3 = []
			.concat(nc.excludeFirst(2).chooseFirst(1))
			.concat(allnot.excludeFirst(2).chooseFirst(1))
			.concat(dn.excludeFirst(2).chooseFirst(1))
			.concat(notall.excludeFirst(2).chooseFirst(1))
			.pairWith("view","stimulus.ejs")
			;

	var block4 = []
			.concat(nc.excludeFirst(3).chooseFirst(1))
			.concat(allnot.excludeFirst(3).chooseFirst(1))
	  	.concat(dn.excludeFirst(3).chooseFirst(1))
    	.concat(notall.excludeFirst(3).chooseFirst(1))
			.pairWith("view","stimulus.ejs")
			;

	var block5 = []
			.concat(nc.excludeFirst(4).chooseFirst(1))
			.concat(allnot.excludeFirst(4).chooseFirst(1))
			.concat(dn.excludeFirst(4).chooseFirst(1))
			.concat(notall.excludeFirst(4).chooseFirst(1))
			.pairWith("view","stimulus.ejs")
			;

	var block6 = []
			.concat(nc.excludeFirst(5).chooseFirst(1))
			.concat(allnot.excludeFirst(5).chooseFirst(1))
	   	.concat(dn.excludeFirst(5).chooseFirst(1))
    	.concat(notall.excludeFirst(5).chooseFirst(1))
			.pairWith("view","stimulus.ejs")
  		;

	var block7 = []
			.concat(nc.excludeFirst(6).chooseFirst(1))
			.concat(allnot.excludeFirst(6).chooseFirst(1))
			.concat(dn.excludeFirst(6).chooseFirst(1))
			.concat(notall.excludeFirst(6).chooseFirst(1))
			.pairWith("view","stimulus.ejs")
			;

			var block8 = []
					.concat(nc.excludeFirst(7).chooseFirst(1))
					.concat(allnot.excludeFirst(7).chooseFirst(1))
		 	   	.concat(dn.excludeFirst(7).chooseFirst(1))
		     	.concat(notall.excludeFirst(7).chooseFirst(1))
					.pairWith("view","stimulus.ejs")
					;

			var block9 = []
					.concat(nc.excludeFirst(8).chooseFirst(1))
					.concat(allnot.excludeFirst(8).chooseFirst(1))
					.concat(dn.excludeFirst(8).chooseFirst(1))
					.concat(notall.excludeFirst(8).chooseFirst(1))
					.pairWith("view","stimulus.ejs")
	    ;

	var block10 = []
			.concat(nc.excludeFirst(9).chooseFirst(1))
			.concat(allnot.excludeFirst(9).chooseFirst(1))
 	   	.concat(dn.excludeFirst(9).chooseFirst(1))
     	.concat(notall.excludeFirst(9).chooseFirst(1))
			.pairWith("view","stimulus.ejs")
			;

	var block11 = []
			.concat(nc.excludeFirst(10).chooseFirst(1))
			.concat(allnot.excludeFirst(10).chooseFirst(1))
			.concat(dn.excludeFirst(10).chooseFirst(1))
			.concat(notall.excludeFirst(10).chooseFirst(1))
			.pairWith("view","stimulus.ejs")
	    ;

	var block12 = []
			.concat(nc.excludeFirst(11).chooseFirst(1))
			.concat(allnot.excludeFirst(11).chooseFirst(1))
 	   	.concat(dn.excludeFirst(11).chooseFirst(1))
     	.concat(notall.excludeFirst(11).chooseFirst(1))
			.pairWith("view","stimulus.ejs")
			;

	var block13 = []
			.concat(nc.excludeFirst(12).chooseFirst(1))
			.concat(allnot.excludeFirst(12).chooseFirst(1))
			.concat(dn.excludeFirst(12).chooseFirst(1))
			.concat(notall.excludeFirst(12).chooseFirst(1))
			.pairWith("view","stimulus.ejs")
	;
	var block14 = []
			.concat(nc.excludeFirst(13).chooseFirst(1))
			.concat(allnot.excludeFirst(13).chooseFirst(1))
			.concat(dn.excludeFirst(13).chooseFirst(1))
			.concat(notall.excludeFirst(13).chooseFirst(1))
			.pairWith("view","stimulus.ejs")
			;

	this.addStaticScreen("getgoing.ejs");
  this.addStaticScreen("consent.ejs");
	this.addStaticScreen("intro.ejs");
//	this.addStaticScreen("trial-message.ejs");
  this.addBlock(sampleItem);
	this.addStaticScreen("begin.ejs")
	this.addBlock(block1);
	this.addBlock(block2);
	this.addBlock(block3);
	this.addBlock(block4);
	this.addBlock(block5);
	this.addBlock(block6);
	this.addBlock(block7);
	this.addBlock(block8);
	this.addBlock(block9);
	this.addBlock(block10);
	this.addBlock(block11);
	this.addBlock(block12);
	this.addBlock(block13);
	this.addBlock(block14);
	this.addStaticScreen("demographic.ejs");
	this.addStaticScreen("finalthanks.ejs");

}
