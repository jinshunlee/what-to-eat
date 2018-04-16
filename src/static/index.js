// pull in desired CSS/SASS files
require( './styles/main.scss' );
var $ = jQuery = require( '../../node_modules/jquery/dist/jquery.js' );           // <--- remove if jQuery not needed
require( '../../node_modules/bootstrap-sass/assets/javascripts/bootstrap.js' );   // <--- remove if Bootstrap's JS not needed 
// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
var div = document.getElementById('main');
var mapDiv = document.getElementById('map');
var map = Elm.Main.embed(div);
map.ports.moveMap.subscribe(function(gmPos) {
    console.log("received", gmPos);
    var myLatlng = new google.maps.LatLng(gmPos);
    gmap.setCenter(myLatlng);
});
var myLatlng = new google.maps.LatLng(0, 0);
var mapOptions = {
  zoom: 6,
  center: myLatlng
};
var gmap = new google.maps.Map(mapDiv, mapOptions);
gmap.addListener('drag', function() {
  var newPos = {
    lat: gmap.getCenter().lat(),
    lng: gmap.getCenter().lng()
  };
  map.ports.mapMoved.send(newPos);
});
