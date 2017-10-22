yaml = require('js-yaml');

exports._safeDump = function(o){
    try {
        var d = yaml.safeDump(o);
        return { success: true, value: d, error: null };
    } catch (e) {
        return { success: false, value: "", error: e.toString() };
    }
};

exports._safeLoad = function(s){
    try {
        return { success: true, value: yaml.safeLoad(s) };
    } catch (e) {
        return { success: false, error: e.toString() };
    }
};
