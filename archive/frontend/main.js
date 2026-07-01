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
        xhr.onload = function() {
            // IE11 doesn't support responseType = 'json'
            var response = JSON.parse(xhr.responseText);
            this.set({
                status: 'connected',
                server: url,
                folders: response.folders,
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
        xhr.onload = function() {
            // IE11 doesn't support responseType = 'json'
            var response = JSON.parse(xhr.responseText);
            this.set('pictures', response.pictures);
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

function TransitionManager() {
    this.$front = $('#picture-wrapper-a');
    this.$back = $('#picture-wrapper-b');
    this.hasPhoto = false;
}

TransitionManager.prototype.forceShowPhoto = function(img) {
    $('.picture-wrapper').empty();
    this.$front.append(img);
    this.hasPhoto = true;
};

TransitionManager.prototype.transitionToPhoto = function(img) {
    if (!this.hasPhoto) {
        this.forceShowPhoto(img);
    } else {
        this.$back.empty().append(img);
        $('#picture-wrapper-a').toggleClass('active');
        var tmp = this.$back;
        this.$back = this.$front;
        this.$front = tmp;
    }
};

function RandomPhotoSequence(pictures) {
    this.pictures = _.shuffle(pictures);
    this.index = 0;
}

RandomPhotoSequence.prototype.getNextPhoto = function() {
    if (this.pictures.length === 0) {
        return undefined;
    }

    var i = this.index;
    this.index = (i + 1) % this.pictures.length;
    return this.pictures[i];
};

function Slideshow(interval, contents) {
    this.transitionManager = new TransitionManager;

    this.interval = interval;
    this.currentImage = undefined;
    this.timerID = undefined;
    
    link(contents, 'pictures', function(_, pictures) {
        this.pictures = new RandomPhotoSequence(pictures);
        this.prepareNextPicture(true);
    }.bind(this));
}

Slideshow.prototype.prepareNextPicture = function(immediate) {
    if (this.currentImage !== undefined) {
        this.currentImage.neutered = true;
        this.currentImage = undefined;
    }

    if (this.timerID !== undefined) {
        clearTimeout(this.timerID);
        this.timerID = undefined;
    }

    var picture = this.pictures.getNextPhoto();
    if (picture === undefined) {
        return;
    }
    var url = $('#server-selection').val() + '/photo?' + $.param({'folder': picture.folder, 'photo': picture.name});

    var cont = function cont() {
        if (img.neutered) {
            return;
        }
        if (immediate) {
            this.transitionManager.forceShowPhoto(img);
        } else {
            this.transitionManager.transitionToPhoto(img);
        }

        this.timerID = setTimeout(function() {
            this.prepareNextPicture(false);
        }.bind(this), this.interval.get('milliseconds'));
    }.bind(this);

    var img = document.createElement('img');
    this.currentImage = img;
    img.addEventListener('load', cont);
    img.addEventListener('error', cont);
    img.addEventListener('abort', cont);
    img.className = 'picture';
    img.src = url;
};

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
    var interval = new Backbone.Model({
        milliseconds: 5000
    });
    var slideshow = new Slideshow(interval, contents);

    // elements
    var $serverSelection = $('#server-selection');
    var $folders = $('.folders');
    var $pictures = $('.pictures');
    var $interval = $('.interval-slider');

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

    function reconnect() {
        var server = $serverSelection.val();
        localStorage.setItem('server', server);
        connection.connect(server);
    }

    $serverSelection.change(function() {
        reconnect();
    });
    $serverSelection.keydown(function(event) {
        if (event.keyCode === 13) {
            reconnect();
        }
    });

    var interval_changed = function() {
        interval.set('milliseconds', Math.floor($interval.val() * 1000));
        $('.interval-text').text($interval.val() + 's');
    };
    $interval.on('input', interval_changed);
    $interval.change(interval_changed);
    $interval.change();

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
    if (previousServer === null) {
        previousServer = document.location.toString();
        if (previousServer[previousServer.length - 1] == '/') {
            previousServer = previousServer.substr(0, previousServer.length - 1);
        }
    }
    if (previousServer !== null) {
        $serverSelection.val(previousServer);
        $serverSelection.change();
    }
})();
