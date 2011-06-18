var fs = require('fs');
var http = require('http');
var url = require('url');
var matches = {};
var scores = {};
var dataFile = '/tmp/data.json';
fs.readFile(dataFile, readHandler) ;

function readHandler (err, data) {
  if (err) {
    console.log(err);
  }
  else {
    var j = JSON.parse(data);
    matches = j[0];
    scores = j[1];
  }
}

function saveScores() {
  fs.writeFile(dataFile, JSON.stringify([matches, scores]));
}

function showScores(res) {
  res.write('<html><body><table>');
  for (var x in scores) {
    res.write('<tr><td>' + x + '</td><td>' + scores[x] + '</td></tr>');
  }
  res.write('</table></body></html>');
}

http.createServer(function (req, res) {
    res.writeHead(200, {'Content-Type': 'text/html'});
    var r = url.parse(req.url, true);
    if (r.pathname === '/match') {
      var win = r.query.win;
      var lose = r.query.lose;
      var name = win + "," + lose;
      if (!matches[name]) {
	matches[name] = 1;
	scores[win] = (scores[win] ? scores[win] : 0) + 1;
	scores[lose] = (scores[lose] ? scores[lose] : 0) - 1;
	saveScores();
      }
      showScores(res);
    }
    
    if (r.pathname === '/') {
      showScores(res);
    }
    res.end('');
  }).listen(1337, 'localhost');
console.log('Server running.');
