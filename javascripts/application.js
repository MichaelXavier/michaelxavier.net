function cursorBlink(interval) {
  var element = document.getElementById('cursor');
  if (element) setInterval(function() {toggle(element);}, interval);
}

function toggle(element) {
  element.style.display = (element.style.display == 'inline') ? 'none' : 'inline';
}
