const electron = require('electron')
const zmq = require('zeromq');

const { app, ipcMain, BrowserWindow } = electron

const child_process = require('child_process')
const fs = require('fs')

const path = require('path')
const url = require('url')

const process_m = require("process")

let mainWindow
let socket
let renderer
let process
let args

function createWindow() {
    //Create a new window
    mainWindow = new BrowserWindow({
        width: 800,
        height: 600
    })
    //Navigate it to index.html
    mainWindow.loadURL(url.format({
        pathname: path.join(__dirname, 'index.html'),
        protocol: 'file:',
        slashes: true
    }))
    //When its closed, reset this variable to null so we dont receive any more events
    mainWindow.on('closed', function() {
        mainWindow = null
    })
}

function startServer() {
    args = []
    process = child_process.spawn(/*path.join(path.dirname(process_m.argv[0]), */"ame-repl"/*)*/, ["tcp://127.0.0.1:8081"], {stdio: 'inherit'})
    // Make a ZMQ socket
    socket = zmq.socket('req');
    // Connect it to the server.
    socket.connect("tcp://127.0.0.1:8081")
    console.log("server started!");

    //Wait for messages from the server.
    socket.on('message', function(msg) {
        console.log(msg.toString());
        //Since commands are processed in-order, the index and callback for this command
        //will be at the top of the list
        let [index, cb] = args.pop();
        // Parse the reply and then send it on to the browser window JS script.
        var obj = JSON.parse(msg.toString());
        renderer.send('reply', obj, index, cb);
    })
}

// When the app is loaded
app.on('ready', function() {
    createWindow();
    startServer();
})

// Quit when all windows are closed.
app.on('window-all-closed', function() {
    app.quit()
})

// When we get a command from the browser window
ipcMain.on('command', (event, command, index, cb) => {
    if (renderer == undefined) {
        // Update the place for us to send the reply to if needed
        renderer = event.sender;
    }
    //add the index and callback to the queue
    args.unshift([index, cb])
    // Send this command on the ZMQ socket to the server.
    socket.send(command);
});

ipcMain.on('restart', (event) => {
    process.kill();
    console.log("killed!");
    startServer();
});
