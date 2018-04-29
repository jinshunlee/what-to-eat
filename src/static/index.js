// pull in desired CSS/SASS files
require( './styles/main.scss' );
var $ = jQuery = require( '../../node_modules/jquery/dist/jquery.js' );           // <--- remove if jQuery not needed
require( '../../node_modules/bootstrap-sass/assets/javascripts/bootstrap.js' );   // <--- remove if Bootstrap's JS not needed 
// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
var div = document.getElementById('main');
var mapDiv = document.getElementById('map');
var map = Elm.Main.fullscreen();
map.ports.moveMap.subscribe(function(gmPos) {
    var myLatlng = new google.maps.LatLng(gmPos);
    gmap.setCenter(myLatlng);
    marker.setPosition(myLatlng)
});
var myLatlng = new google.maps.LatLng(1.3030487878, 103.825808363);
var mapOptions = {
  zoom: 17.9,
  center: myLatlng
};
var gmap = new google.maps.Map(mapDiv, mapOptions);
var marker = new google.maps.Marker({
  position: myLatlng,
  title: "Current Location"
});
marker.setMap(gmap);
