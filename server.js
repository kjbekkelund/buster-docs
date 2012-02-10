var http = require("http");
var fs = require("fs");
var path = require("path");
var url = require("url");
var mime = require("mime");
var ejs = require("ejs");

var PUBLIC = path.join(__dirname, "public");
var SITE = path.join(__dirname, "site");
var LAYOUT = fs.readFileSync(path.join(__dirname, "layout.html"), "utf8");

function serveFile(path, res) {
    fs.readFile(path, function (err, data) {
        res.writeHead(200, {"Content-Type": mime.lookup(path)});
        res.write(data);
        res.end();
    });
}

function serveTemplate(path, pathname, res) {
    if (pathname.slice(pathname.length - 1) == "/") {
        res.writeHead(200, {"Content-Type": "text/html"});
        fs.readFile(path, function (err, data) {
            res.write(renderTemplate(data.toString("utf8")));
        res.end();
        });
    } else {
        res.writeHead(301, {"Location": pathname + "/"});
        res.end();
    }
}

function renderTemplate(content) {
    return ejs.render(LAYOUT, {content: content});
}

function notFound(res) {
    res.writeHead(404);
    res.write("404 Page Not Found");
    res.end();
}

var server = http.createServer(function (req, res) {
    var u = url.parse(req.url);
    var pubFilePath = path.join(PUBLIC, u.pathname);
    fs.stat(pubFilePath, function (err, stat) {
        if (err || !stat.isFile()) {
            var dirPath = path.join(SITE, u.pathname);
            fs.stat(dirPath, function (err, stat) {
                if (!err && stat.isDirectory()) {
                    var indexHtmlPath = path.join(dirPath, "index.html");
                    fs.stat(indexHtmlPath, function (err, stat) {
                        if (err || !stat.isFile()) {
                            notFound(res);
                        } else {
                            serveTemplate(indexHtmlPath, u.pathname, res);
                        }
                    });
                } else {
                    var templatePath = dirPath.slice(0, dirPath.length - 1) + ".html";
                    fs.stat(templatePath, function (err, stat) {
                        if (!err && stat.isFile()) {
                            serveTemplate(templatePath, u.pathname, res);
                        } else {
                            notFound(res);
                        }
                    });
                }
            });
        } else {
            serveFile(pubFilePath, res);
        }
    });
});

exports.start = function (port) {
    port = port || parseInt(process.argv[2], 10) || 8090;
    server.listen(port);
    console.log("Listening on http://127.0.0.1:" + port); 
    return server;
};
