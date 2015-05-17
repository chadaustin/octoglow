var ServerConnection = Backbone.Model.extend({
    initialize: function() {
        this.currentXHR = null;

        this.set({
            status: 'disconnected',
            server: null,
            folders: [],
        });
    },

    connect: function(url) {
        if (this.currentXHR) {
            this.currentXHR.abort();
            this.currentXHR = null;
        }

        this.set('status', 'connecting...')
        var xhr = new XMLHttpRequest;
        xhr.open('GET', url + '/folders');
        xhr.responseType = 'json';
        xhr.onload = function() {
            this.set({
                status: 'connected',
                server: url,
                folders: xhr.response.folders,
            });
        }.bind(this);
        xhr.onerror = function() {
            this.set({
                status: 'failed',
                folders: [],
            });
        }.bind(this);
        xhr.onabort = function() {
            this.set({
                status: 'aborted',
                folders: [],
            });
        }.bind(this);
        xhr.send();
        this.currentXHR = xhr;
    },
});

var FolderContents = Backbone.Model.extend({
    constructor: function(connection, currentFolder) {
        this.connection = connection;
        this.currentFolder = currentFolder;
        Backbone.Model.apply(this, arguments);

        this.connection.on('change:server', this._update, this);
        this.currentFolder.on('change:folder', this._update, this);

        this._update();
    },

    initialize: function() {
        this.set({
            pictures: [],
        });

        this.currentXHR = null;
    },

    _update: function() {
        if (this.currentXHR) {
            this.currentXHR.abort();
            this.currentXHR = null;
        };

        var server = this.connection.get('server');
        if (!this.server) {
            this.set('pictures', []);
        }

        var folder = this.currentFolder.get('folder');
        if (folder == null) {
            this.set('pictures', []);
            return;
        }

        var xhr = new XMLHttpRequest;
        xhr.open('GET', server + '/contents?' + $.param({'folder': folder}));
        xhr.responseType = 'json';
        xhr.onload = function() {
            this.set('pictures', xhr.response.pictures);
        }.bind(this);
        xhr.onerror = function() {
            this.set('pictures', []);
        }.bind(this);
        xhr.onabort = function() {
            this.set('pictures', []);
        }.bind(this);
        xhr.send();
        this.currentXHR = xhr;
    },
});

function Slideshow(contents) {
    this.interval = 5000;

    $('#picture-wrapper-a').on('transitionend', function() {
        this.prepareNextPicture();
    }.bind(this));

    link(contents, 'pictures', function(_, pictures) {
        var oldPictures = this.pictures;
        this.pictures = pictures;
        if ((oldPictures === undefined || oldPictures.length === 0) && pictures.length > 0) {
            this.prepareNextPicture();
        }
    }.bind(this));
}

Slideshow.prototype.prepareNextPicture = function() {
    var active = $('#picture-wrapper-a').hasClass('active');
    var $next = $(active ? '#picture-b' : '#picture-a');

    // random
    var picture = this.pictures[Math.floor(Math.random() * this.pictures.length)];
    if (picture === undefined) {
        debugger;
    }
    
    var url = $('#server-selection').val() + '/photo?' + $.param({'folder': picture.folder, 'photo': picture.name});
    $next.attr({'src': url});
    $next.one('load', function() {
        setTimeout(function() {
            $('#picture-wrapper-a').toggleClass('active');
        }.bind(this), this.interval);
    }.bind(this));
}

function link(model, field, handler) {
    model.on('change:' + field, handler);
    handler(model, model.get(field));
}

(function() {
    // application models
    var connection = new ServerConnection;
    var currentFolder = new Backbone.Model({
        folder: null
    });
    var contents = new FolderContents(connection, currentFolder);
    var slideshow = new Slideshow(contents);

    // elements
    var $serverSelection = $('#server-selection');
    var $folders = $('.folders');
    var $pictures = $('.pictures');

    link(connection, 'status', function(_, status) {
        $('.server-status-text').text(status);
        $folders.prop('disabled', status !== 'connected');
        $('#include-subfolders').prop('disabled', status !== 'connected');
    });

    link(connection, 'folders', function(_, folders) {
        $folders.empty();
        folders.forEach(function(folder) {
            var option = $('<option>');
            option.attr({'value': folder}).text(folder);
            $folders.append(option);
        });

        updateCurrentFolder();
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
