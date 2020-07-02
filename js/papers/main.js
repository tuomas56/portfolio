window.selectRandom = () => {
    updateListing()
    var items = $("#target-list > li");
    if (items.length > 0) {
        var item = items[Math.floor(Math.random()*items.length)];
        console.log('items")')
        $("#target-list").html('')
        $("#target-list").append(item)
    }
}

$(() => {

    $('.fancy-select').select2();

    $(".sticky-bar").stick_in_parent();

    $(".sticky-bar").on("sticky_kit:stick", () => {
        $(".sticky-bar").addClass("js-is-sticky")
        $(".sticky-bar").addClass("js-has-stuck")
    });

    $(".sticky-bar").on("sticky_kit:unstick", () => {
        $(".sticky-bar").removeClass("js-is-sticky")
    });

    var xhr = new XMLHttpRequest();
    xhr.open('GET', '/papers.db', true);
    xhr.responseType = 'arraybuffer';

    window.db = null;
    xhr.onload = function(e) {
        var uInt8Array = new Uint8Array(this.response);
        window.db = new SQL.Database(uInt8Array);
        updateSelectBox();
        loading_screen.finish();
    };
    xhr.send();

    $(".fancy-select").on('select2:select', updateSelectBox);
    $(".fancy-select").on('select2:unselect', updateSelectBox);

    $("#select-random").click(selectRandom);
})

function fboard(b) {
    if (b.length <= 3) {
        return b.toUpperCase()
    } else {
        return b.charAt(0).toUpperCase() + b.slice(1)
    }
}

function fmodule(b) {
    return b.toUpperCase();
}

function fname(b) {
    return b
}

window.updateSelectBox = () => {
    boards = $("#select-board").select2('data');
    modules = $("#select-module").select2('data');
    papers = $("#select-paper").select2('data');

    if (boards.length == 0) {
        stmt = 'select distinct board from papers'
        if (modules.length > 0) {
            stmt += ' where module in ('
            mns = modules.map((val) => "'" + val.id + "'")
            stmt += mns.join(', ')
            stmt += ')'

            if (papers.length > 0) {
                stmt += ' and name in ('
                mns = papers.map((val) => "'" + val.id + "'")
                stmt += mns.join(', ')
                stmt += ')'
            }
        } else {
            if (papers.length > 0) {
                stmt += ' where name in ('
                mns = papers.map((val) => "'" + val.id + "'")
                stmt += mns.join(', ')
                stmt += ')'
            }
        }
        stmt += ' order by board'
        console.log(stmt)
        res = db.exec(stmt)[0].values
        console.log(res)
        $("#select-board").html('')
        for (a of res) {
            $("#select-board").append(new Option(fboard(a[0]), a[0], false, false))
        }
        $('#select-board').trigger('change')
    }

    if (modules.length == 0) {
        stmt = 'select distinct module from papers'
        if (boards.length > 0) {
            stmt += ' where board in ('
            mns = boards.map((val) => "'" + val.id + "'")
            stmt += mns.join(', ')
            stmt += ')'

            if (papers.length > 0) {
                stmt += ' and name in ('
                mns = papers.map((val) => "'" + val.id + "'")
                stmt += mns.join(', ')
                stmt += ')'
            }
        } else {
            if (papers.length > 0) {
                stmt += ' where name in ('
                mns = papers.map((val) => "'" + val.id + "'")
                stmt += mns.join(', ')
                stmt += ')'
            }
        }
        stmt += ' order by module'
        console.log(stmt)
        res = db.exec(stmt)[0].values
        console.log(res)
        $("#select-module").html('')
        for (a of res) {
            $("#select-module").append(new Option(fmodule(a[0]), a[0], false, false))
        }
        $('#select-module').trigger('change')
    }

    if (papers.length == 0) {
        stmt = 'select distinct name from papers'
        if (modules.length > 0) {
            stmt += ' where module in ('
            mns = modules.map((val) => "'" + val.id + "'")
            stmt += mns.join(', ')
            stmt += ')'

            if (boards.length > 0) {
                stmt += ' and board in ('
                mns = boards.map((val) => "'" + val.id + "'")
                stmt += mns.join(', ')
                stmt += ')'
            }
        } else {
            if (boards.length > 0) {
                stmt += ' where board in ('
                mns = boards.map((val) => "'" + val.id + "'")
                stmt += mns.join(', ')
                stmt += ')'
            }
        }
        stmt += ' order by name'
        console.log(stmt)
        res = db.exec(stmt)[0].values
        console.log(res)
        $("#select-paper").html('')
        for (a of res) {
            $("#select-paper").append(new Option(fname(a[0]), a[0], false, false))
        }
        $('#select-paper').trigger('change')
    }

    updateListing()
}

function flink(qp, r) {
    if (qp) {
        return `<a class='float-right btn' href="${r}">Question Paper</a>`
    } else {
        return `<a class='float-right btn' href="${r}">Mark Scheme</a>`
    }
}

function fentry(r) {
    return "<span class='list-span'>" + fboard(r[0]) + " " + fmodule(r[1]) + " " + fname(r[2]) + "</span>" + flink(false, r[4]) + flink(true, r[3])
}

window.updateListing = () => {
    $("#target-list").html('')
    
    boards = $("#select-board").select2('data');
    modules = $("#select-module").select2('data');
    papers = $("#select-paper").select2('data');

    stmt = 'select board, module, name, qp_link, ms_link from papers'
    aps = []

    if (boards.length > 0) {
        s = '('
        s += boards.map((val) => "'" + val.id + "'").join(", ")
        s += ')'
        aps.push('board in ' + s)
    }

    if (modules.length > 0) {
        s = '('
        s += modules.map((val) => "'" + val.id + "'").join(", ")
        s += ')'
        aps.push('module in ' + s)
    }

    if (papers.length > 0) {
        s = '('
        s += papers.map((val) => "'" + val.id + "'").join(", ")
        s += ')'
        aps.push('name in ' + s)
    }

    if (aps.length > 0) {
        stmt += ' where '
        stmt += aps.join(' and ')
    }

    stmt += ' order by board, module, name asc'

    console.log(stmt)

    res = db.exec(stmt)[0].values

    for (r of res) {
        $("#target-list").append("<li class='list-group-item bg-light'>" + fentry(r) + "</li>")
    }
}

