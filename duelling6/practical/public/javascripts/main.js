jQuery(function($) {

	/**
	 * Setup scene
	 */
  var people = ['Andreas_Brudin.jpg','Anna_Otto.jpg','PEROold.jpg','a-jansson.jpg','anbl.jpg','anbr.jpg','anda.jpg','anders.jpg','andl.jpg','anek.jpg','anfr.jpg','ange.jpg','anhe.jpg','anho.jpg','anka.jpg','anlu.jpg','anma.jpg','anoh.jpg','anot.jpg','anpe.jpg','anso.jpg','ansv.jpg','ante.jpg','anth.jpg','anwe.jpg','anwo.jpg','arsi.jpg','arsy.jpg','arta.jpg','bern.jpg','bjho.jpg','bjro.jpg','bosj.jpg','cale.jpg','calle.jpg','caoz.jpg','chda.jpg','chen.jpg','chhi.jpg','chle.jpg','chmo.jpg','chwi.jpg','cosa.jpg','dabo.jpg','dada.jpg','daer.jpg','daja.jpg','dalu.jpg','dana.jpg','dape.jpg','dast.jpg','davi.jpg','davo.jpg','djhe.jpg','edwi.jpg','er.jpg','ergu.jpg','erik.jpg','erlu.jpg','erol.jpg','ersk.jpg','eva.jpg','ey.jpg','fesp.jpg','fibo.jpg','frda.jpg','fredrik.jpg','frel.jpg','frjo.jpg','frlo.jpg','frme.jpg','guso.jpg','haca.jpg','hada.jpg','hala.jpg','hata.jpg','heed.jpg','hehe_melvin.jpeg','henu.jpg','heol.jpg','hesv.jpg','hhan.jpg','hjmo.jpg','ho.jpg','hoch.jpg','hs.jpg','inga.jpg','jada.jpg','jalo.jpg','jaol.jpg','jeca.jpg','jela.jpg','jeli.jpg','jenl.jpg','jera.jpg','jgh.jpg','jiha.jpg','jipe.jpg','jm.jpg','joal.jpg','jobe.jpg','jobj.jpg','joby.jpg','joel.jpg','joen.jpg','joha.jpg','joho.jpg','jojk.jpg','joli.jpg','jolm.jpg','jolo.jpg','jolu.jpg','jonb.jpg','jono.jpg','joon.jpg','jose.jpg','josi.jpg','jost.jpg','jowi.jpg','kaan.jpg','kali.jpg','keal.jpg','keho.jpg','kn.jpg','kran.jpg','krli.jpg','krsi.jpg','laer.jpg','lajo.jpg','lero.jpg','liha.jpg','liho.jpg','lisu.jpg','liwi.jpg','ll.jpg','lohj.jpg','lola.jpg','maan.jpg','maav.jpg','mabe.jpg','mabj.jpg','mach.jpg','mafr.jpg','magl.jpg','magnus.jpg','magu.jpg','maha.jpg','majo.jpg','mako.jpg','mali.jpg','mami.jpg','many.jpg','marc.jpg','mare.jpg','mart.jpg','marten.jpg','masa.jpg','masj.jpg','mats.jpg','mawe.jpg','mawi.jpg','me.jpg','mfri.jpg','miha.jpg','mipa.jpg','mire.jpg','mist.jpg','miwi.jpg','moha.jpg','motr.jpg','nila.jpg','nima.jpg','nisk.jpg','olsu.jpg','olsv.jpg','osca.jpg','oser.jpg','pala.jpg','pali.jpg','pape.jpg','peal.jpg','peli.jpg','pemo.jpg','perg.jpg','peso.jpg','phek.jpg','pm.jpg','pn.jpg','pobe.jpg','raha.jpg','roku.jpg','rolu.jpg','rove.jpg','saev.jpg','sefa.jpg','siau.jpg','siha.jpg','sm.jpg','soek.jpg','steko.jpg','stju.jpg','stla.jpg','stpe.jpg','tejo.jpg','than.jpg','thomas.jpg','tobr.jpg','tocl.jpg','toja.jpg','tolu.jpg','trin.jpg','ulak.jpg','ulf.jpg','ulli.jpg','vevu.jpg','vipa.jpg','wiol.jpg','ylfr.jpg'];
  var facts = {
		competence: ["Java", ".NET", "MySQL", "Bakning", "Upphopp", "Tresteg", "Torkning"],
		creativity: ["Natural leader", "Painter", "Singstar hero", "Fussball master", "Beerpong dude", "Parcour"],
		business: ["Sm&aring;l&auml;nning", "Social anthropologist", "Syr sina egna kl&auml;der", "Stockbroker", "B&auml;st"]
	};
	var scene = new Scene($('#container'), {
		fps: 60,
		controls: false,
		stats: false
	});

	// Start scene
	scene.start();

	/**
	 * Setup global instances
	 */
	var searchImages = [];

	/**
	 * Setup SocketIO client
	 */

	var socket = new io.Socket();
	//socket.connect();

	socket.on('connect', function() {
		console.log('connected');
		$('body').css({
			'border-top': '3px solid green'
		})
		setTimeout(function() {
			$('body').css({
				'border': 'none'
			})
		},
		1000)
	});

	socket.on('message', function(data) {
		console.debug('received message');
		console.debug(data);

		if (data.action) {
			console.info('received action: %s', data.action.type);

			if (data.action.type === 'image') {
        for (var i = 0, n = people.length; i < n; i++) {
					var person = people[i] + "";
					
						var passing = new Passing("images/people/" + person, {
							duration: 15000 * Math.random(),
							onImageLoad: function(img) {
								var height = img.height;
								passing.scale(200 / img.height);
							}
						});
						passing.add(scene);
						passing.scale(0.2);
					
        }

			} else if (data.action.type === 'background') {
				$('#backdrop').attr('src', data.action.url);

			} else if (data.action.type === 'search') {
				// Rewrite some search queries
				var query = data.action.query.replace(/\blucia\b/g, 'jul lucia');

				// Take first five words
				query = query.split(/\s+/).slice(0, 5).join(' ');

				Search.search(query, function(result) {
					if (result) {
						console.log('create Passing from search image');
						var obj = new Passing(result.url, {
							width: result.width,
							height: result.height,
							duration: 10000,
							onImageLoad: function() {
								console.log('search image loaded');
								obj.add(scene);
								obj.scale(5);
								searchImages.push(obj);
							},
							onComplete: function() {
								var index = searchImages.indexOf(obj);
								if (index !== 1) {
									searchImages.splice(index, 1);
								}
							}
						})
					}
				})

			} else if (data.action.type === 'remove-search') {
				searchImages.forEach(function(obj) {
					obj.remove(scene);
				});
				searchImages = [];

			} else {
				console.info('unknown action: %s', data.action.type);

			}

		} else {
			// unknown message
		}

	});

	socket.on('disconnect', function() {
		console.log('disconnected');
		$('body').css({
			'border-top': '5px solid red'
		})
	});

	/**
	 * Setup mouse events
	 */

	var mouseStartX = 0;
	var mouseStartY = 0;

	document.addEventListener('mousedown', function(event) {
		mouseStartX = event.clientX;
		mouseStartY = event.clientY;
		document.addEventListener('mousemove', moveCamera, false);
	},
	false);

	document.addEventListener('mouseup', function() {
		document.removeEventListener('mousemove', moveCamera, false);
		scene.resetCamera();
	},
	false);

	$('#props').keyup(function(e) {
		if (e.keyCode === 13) {
			$('#selectedPerson').remove();
			$('#selectedList').remove();
			var el = $(e.target);
			$('#propsList').append('<li>' + el.val() + '</li>');
			el.val('');
		}
		return false;
	});
	
	$('#whodat').click(function(e) {
		var selected;
		var i;
		var max = 10;
		var selectedHeight, selectedWidth;
		
		var p = function() {
				var person = people[Math.round(Math.random() * (people.length - 1))] + "";
			  var height, width;
				var passing = new Passing("images/people/" + person, {
					duration: 4000,
					onImageLoad: function(img) {
						height = img.height;
						width = img.width;
						passing.scale(800 / img.height);
					}
				});
				passing.add(scene);
				passing.scale(1);
				
				if (Math.random() > 0.7 || (i === (max - 1))) { 
					selected = "images/people/" + person;
					selectedHeight = height;
					selectedWidth = width;
				}
		}
		for (i = 0, n = max; i < n; i++) {
			setTimeout(p, i * 1000);
    }

		var displayPic = function() {
			var image = '<img id="selectedPerson" src="' + selected + '" style="position: absolute; top: 150px; left: 40%; border: 1px solid; height:' + 
				400 + '; width: ' + selectedWidth * (400 / selectedHeight) + ';"/>';
			var data = '<ul id="selectedList"><li>Competence: ' + facts.competence[Math.round((facts.competence.length - 1) * Math.random())] + "</li>" + 
				"<li>Creativity: " + facts.creativity[Math.round((facts.creativity.length - 1) * Math.random())] + "</li>" +
				"<li>Business sense: " + facts.business[Math.round((facts.business.length - 1) * Math.random())] + "</li></ul>";
				
			$('body').append(image);
			$('body').append(data);
		}

		setTimeout(displayPic, (i + 2)*1000);

		$('#propsList').empty();
	});

	function moveCamera(event) {
		mouseX = event.clientX - mouseStartX;
		mouseY = event.clientY - mouseStartY;
		scene.shiftCamera(mouseX, mouseY);
	}

	// Start initial anim
	//$('#backdrop').attr('src', '/images/backdrop4.jpg');
});
