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
    xhr.responseType = 'json';
    xhr.open('GET', url + '/folders');
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

    var xhr = new XMLHttpRequest;
    xhr.responseType = 'json';
    xhr.open('GET', server + '/contents?' + $.param({'folder': folder}));
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

(function() {
    function link(model, field, handler) {
        model.on('change:' + field, handler);
        handler(model, model.get(field));
    }

    var connection = new ServerConnection;
    var contents = new FolderContents;
    
    link(connection.status, 'status', function(_, status) {
        $('.server-status-text').text(status);
        $('.folders').prop('disabled', status !== 'connected');
        $('.include-subfolders').prop('disabled', status !== 'connected');
    });

    link(connection.status, 'folders', function(_, folders) {
        $('.folders').empty();
        folders.forEach(function(folder) {
            var option = $('<option>');
            option.attr({'value': folder}).text(folder);
            $('.folders').append(option);
        });
    });

    link(contents.pictures, 'pictures', function(_, pictures) {
        var folder = $($('.folders option:selected')[0]).text();
        
        $('.pictures').empty();
        pictures.forEach(function(picture) {
            var img = $('<img>');
            img.attr({'src': $('#server-selection').val() + '/photo?' + $.param({'folder': folder, 'photo': picture.name})});
            $('.pictures').append(img);
        });
    });

    $('#server-selection').change(function() {
        connection.connect($(this).val());
    });

    $('.folders').change(function() {
        var selected = $('.folders option:selected');
        contents.update($('#server-selection').val(), $(selected[0]).text());
    });
})();
