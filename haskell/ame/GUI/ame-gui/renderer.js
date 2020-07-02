const { ipcRenderer, remote } = require('electron');
const dialog = remote.dialog;
const fs = remote.require('fs');

// Here there is a set of Card classes which can be used to build cards to display input and output
// Card is the base class and it accepts a Content class, which is some kind of Panel (TextPanel etc)
// The card has two methods, generate and typeset, for generating the html, and a hook to be executed when it is
// typeset.

// When writing a card
function write_card(card, index) {
    console.log(card);
    if (index !== undefined) {
        //If we have an index, look for the corresponding input card and then go up
        //two levels and append after that.
        $($("#card-container .input-card").get(index - 1)).parent().parent().after(card.generate())
    } else {
        //Otherwise append the generated html to the end of the card container
        $("#card-container").append(card.generate());
    }
    // Call the typeset hook
    card.typeset();
}

function Card(content, float, classes) {
    this.content = content
    this.classes = classes || "";
    this.float = float || "right";
    // A regular card just applies the classes its given
    // And adds float right if there is no float supplied, otherwise the user option is used.
    this.generate = function() {
        return `
        <div class="row justify-content-${this.float == "right" ? "end" : "start"}">
            <div class="col-8">
                <div class="card ${this.classes}">
                    ${this.content.generate()}
                </div>
            </div>
        <\div><br/>
        `
    }

    this.typeset = function() {
        this.content.typeset();
    }

    return this;
}

// A TextPanel is a simple panel that displays raw text in the card body
// and optionally has extra classes added.
function TextPanel(text, classes) {
    this.text = text;
    this.classes = classes || "";

    this.generate = function() {
        return `
        <div class="card-body ${this.classes}">
            ${this.text}
        </div>
        `
    }

    this.typeset = function() {}
    return this;
}

// MathJaxPanel wraps the input in $$ so that it is typeset
// by MathJax as Latex
function MathJaxPanel(text) {
    // Make this a TextPanel where the text is wrapped with $$
    TextPanel.bind(this)("$$" + text + "$$");
    // When typesetting, tell MathJax that it should redraw the page.
    this.typeset = function() {
        MathJax.Hub.Typeset();
    }
    return this;
}

// The CodePanel is similar to a TextPanel but uses the <code> element
// instead of <div> so that the output uses a monospace font.
function CodePanel(text) {
    this.text = text;

    this.generate = function() {
        return `<code class="card-body bg-dark text-light">${this.text}</code>`
    }

    this.typeset = function() {}

    return this;
}

// An InputCard is a card with bg-dark to give a dark background,
// that floats to the left, and has a single code panel with the given text.
function InputCard(text) {
    return Card.bind(this)(new CodePanel(text), "left", "bg-dark input-card")
}

// A ValueCard displays a latex value using MathJax
// And it floats right.
function ValueCard(text) {
    return Card.bind(this)(new MathJaxPanel(text))
}

// ErrorCards have a red border and red text
// with a text panel with the error message in.
function ErrorCard(text) {
    return Card.bind(this)(new TextPanel(text, "text-danger"), "right", "border-danger")
}

//For { "type": "list" } results
function ListPanel(vals) {
    this.vals = vals;

    this.generate = function() {
        res = []
        for (val of this.vals) {
            // Make a list group item for each value, wrapping the value in $ $ for inline MathJax
            res.push(`<li class="list-group-item">$${val}$</li>`)
        }
        // The put the whole thing in a list group
        return `<ul class="list-group-flush card-body">${res.join("\n")}</ul>`
    }

    this.typeset = function() {
        MathJax.Hub.Typeset();
    }

    return this
}

function TablePanel(vars, vals) {
    this.vars = vars;
    this.vals = vals;

    this.generate = function() {
        //Start of with a table, and start a row in the table header
        res = ["<table class=\"table card-body\">", "<thead>", "<tr>"]
        for (v of this.vars) {
            // Add a heading for each variable
            res.push(`<th scope="col">$${v}$</th>`)
        }
        //End the row and the header, and start the body
        res.push("</tr>")
        res.push("</thead>")
        res.push("<tbody>")
        //Transpose the list of values so instead of having a list of columns we have a list of rows
        var tvals = this.vals[0].map((col, i) => this.vals.map(row => row[i]));
        for (row of tvals) {
            //Start a row
            res.push("<tr>")
            for (e of row) {
                //Make an element for every value in the row
                res.push(`<td>$${e}$</td>`)
            }
            //End the row
            res.push("</tr>")
        }
        //End the body and the table
        res.push("</tbody>")
        res.push("</table>")
        return res.join("\n")
    }

    this.typeset = function() {
        MathJax.Hub.Typeset();
    }

    return this
}

function needs_to_scroll() {
    var $container = $("#card-container");
    //If the scrolled distance, minus the initial scroll distance
    // is equal to the height, it means we have scrolled to the bottom of the window,
    // so we need to automatically scroll it when more cards are added
    return $container.prop("scrollHeight") - $container.scrollTop() == $container.outerHeight();
}

function do_scroll(needed) {
    if (needed) {
        var $container = $("#card-container");
        // If we need to, set the intial scroll value to the value we just calculated.
        $container.scrollTop($container.prop("scrollHeight") - $container.outerHeight())
    }
}

// Whenever we get a reply back from the server.
ipcRenderer.on('reply', (event, obj, index, cb) => {
    // needs_to_scroll and do_scroll must be seperate
    // because we need to check if we're at the bottom
    // BEFORE we add new stuff.
    var needed = needs_to_scroll();

    switch (obj.type) {
        case "value":
            write_card(new ValueCard(obj.value), index);
            break;
        case "error":
            write_card(new ErrorCard(obj.error), index);
            break;
        case "list":
            write_card(new Card(new ListPanel(obj.values)), index);
            break;
        case "table":
            write_card(new Card(new TablePanel(obj.vars, obj.vals)), index);
            break;
    }

    do_scroll(needed);

    //If we have requested a callback on this card, execute it.
    if (cb) {
        console.log("callback");
        reply_callback()
    }
});

let curPath = null;
let input = [];
let reply_callback;

// Whenever we press enter while typing in the main input
$("#input-form").submit(function(evt) {
    // Prevent this event from bubbling up and causing a page refresh
    evt.preventDefault();
    var needed = needs_to_scroll();
    var x = $("#main-input").val();
    // Make a new input card with the value of the main input
    var card = new InputCard(x);
    write_card(card);
    //Save the command to the input log
    input.push(x);
    let index = input.length;
    // Write the card to the screen and then send the input off to the server.
    ipcRenderer.send('command', x, index);
    do_scroll(needed);
    $("#main-input").val("");
});

$("#new").click(function(evt) {
    evt.preventDefault();
    //Restarting the server clears the environment
    ipcRenderer.send("restart");
    //Remove all the cards
    $("#card-container").children().remove()
    //We don't have a file open so reset curPath
    curPath = null;
    //Reset the input log
    input = []
})

$("#open").click(function(evt) {
    evt.preventDefault();
    //Show a file dialog
    dialog.showOpenDialog({
        title: 'Open File',
        filters: [{
            //Only allow opening .ame files
            name: 'AME Files', extensions: ['ame']
        }],
        properties: ['openFile']
    }, ([path]) => {
        //Restarting the server, remove the existing cards
        ipcRenderer.send("restart");
        $("#card-container").children().remove()
        //Read the file and parse it as JSON.
        //It should be a list of strings to feed to the server
        let vals = JSON.parse(fs.readFileSync(path))
        //Set the current path we're editing
        curPath = path
        input = []
        //Setup a fake MathJax Hub so that we dont call Typeset
        //Too many times - calling it too fast will cause some glitches
        //Where a value is duplicated.
        var realhub = MathJax.Hub;
        MathJax.Hub = {
            Typeset: function() {}
        }
        for (let [i, val] of vals.entries()) {
            //Write the input card and add the input to the log
            write_card(new InputCard(val))
            input.push(val)
            if (i < vals.length - 1) {
                //If this is not the last one, send a normal command
                ipcRenderer.send("command", val, input.length, false);
            } else {
                //If this is the last one, add a callback for when the card is done
                //Having a global callback variable for this is a bit hacky but neccessary
                //because i cant pass functions from the renderer to the main process and then back
                reply_callback = function() {
                    //When the last one is added restore the hub to the real hub
                    MathJax.Hub = realhub;
                    //Typeset the whole thing
                    realhub.Typeset();
                    //And scroll to the bottom
                    do_scroll(true);
                }
                ipcRenderer.send("command", val, input.length, true);
            }
        }
    })
})

$("#save").click(function(evt) {
    evt.preventDefault();
    //If we are not editing a specific path,
    //This behaves like a save-as
    if (curPath == null) {
        $("#save-as").click();
    } else {
        //Otherwise just serialise the input log to JSON
        //and write it to the file
        fs.writeFileSync(curPath, JSON.stringify(input));
    }
})

$("#save-as").click(function(evt) {
    evt.preventDefault();
    dialog.showSaveDialog({
        title: 'Save File',
        filters: [{
            name: 'AME Files', extensions: ['ame']
        }]
    }, (path) => {
        //Pick a path and save the input log to it.
        fs.writeFileSync(path, JSON.stringify(input));
        //Save this as the current path, so further save operations will not open a dialog.
        curPath = path;
    })
})