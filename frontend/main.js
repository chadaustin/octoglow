function ServerConnection() {
    this.status = new Backbone.Model({
        status: 'disconnected',
        folders: [],
    });

    this.currentXHR = null;
}

ServerConnection.prototype.connect = function(url) {
    if (this.currentXHR) {
        this.currentXHR.abort();
        this.currentXHR = null;
    }

    var xhr = new XMLHttpRequest;
    xhr.open('GET', url + '/folders');
    xhr.responseType = 'json';
    xhr.onload = function() {
        this.status.set('status', 'connected');
        this.status.set('folders', xhr.response.folders);
    }.bind(this);
    xhr.onerror = function() {
        this.status.set('status', 'failed');
        this.status.set('folders', []);
    }.bind(this);
    xhr.onabort = function() {
        this.status.set('status', 'aborted');
        this.status.set('folders', []);
    }.bind(this);
    xhr.send();
    this.currentXHR = xhr;
};

function FolderContents() {
    this.pictures = new Backbone.Model({
        'pictures': [],
    });

    this.currentXHR = null;
}

FolderContents.prototype.update = function(server, folder) {
    if (this.currentXHR) {
        this.currentXHR.abort();
        this.currentXHR = null;
    };

    if (folder === null) {
        this.pictures.set('pictures', []);
        return;
    }

    var xhr = new XMLHttpRequest;
    xhr.open('GET', server + '/contents?' + $.param({'folder': folder}));
    xhr.responseType = 'json';
    xhr.onload = function() {
        this.pictures.set('pictures', xhr.response.pictures);
    }.bind(this);
    xhr.onerror = function() {
        this.pictures.set('pictures', []);
    }.bind(this);
    xhr.onabort = function() {
        this.pictures.set('pictures', []);
    }.bind(this);
    xhr.send();
    this.currentXHR = xhr;
};

function link(model, field, handler) {
    model.on('change:' + field, handler);
    handler(model, model.get(field));
}

(function() {
    // application models
    var connection = new ServerConnection;
    var contents = new FolderContents;
    var currentFolder = new Backbone.Model({
        folder: null
    });

    // elements
    var $serverSelection = $('#server-selection');
    var $folders = $('.folders');
    var $pictures = $('.pictures');

    link(connection.status, 'status', function(_, status) {
        $('.server-status-text').text(status);
        $folders.prop('disabled', status !== 'connected');
        $('#include-subfolders').prop('disabled', status !== 'connected');
    });

    link(connection.status, 'folders', function(_, folders) {
        $folders.empty();
        folders.forEach(function(folder) {
            var option = $('<option>');
            option.attr({'value': folder}).text(folder);
            $folders.append(option);
        });

        updateCurrentFolder();
    });

    link(currentFolder, 'folder', function(_, folder) {
        contents.update($serverSelection.val(), folder);
    });

    link(contents.pictures, 'pictures', function(_, pictures) {
        var folder = currentFolder.get('folder');
        if (pictures.length > 0) {
            var picture = pictures[0];
            var url = $serverSelection.val() + '/photo?' + $.param({'folder': folder, 'photo': picture.name});
            $('#current-picture').attr({'src': url});
        }

        if (pictures.length > 1) {
            var picture = pictures[1];
            var url = $serverSelection.val() + '/photo?' + $.param({'folder': folder, 'photo': picture.name});
            $('#next-picture').attr({'src': url});
        }

        /*
        $pictures.empty();
        pictures.forEach(function(picture) {
            var img = $('<img>');
            img.attr({'src': $serverSelection.val() + '/photo?' + $.param({'folder': folder, 'photo': picture.name})});
            $pictures.append(img);
        });
        */
    });

    $serverSelection.change(function() {
        var server = $(this).val();
        localStorage.setItem('server', server);
        connection.connect(server);
    });

    function updateCurrentFolder() {
        var selected = $('.folders option').filter(':selected');
        if (selected.length) {
            currentFolder.set('folder', $(selected[0]).text());
        } else {
            currentFolder.set('folder', null);
        }
    }
    
    $folders.change(function() {
        updateCurrentFolder();
    });



    // load previous state
    var previousServer = localStorage.getItem('server');
    if (previousServer !== null) {
        $serverSelection.val(previousServer);
        $serverSelection.change();
    }    
})();
