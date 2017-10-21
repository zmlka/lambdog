yaml = require('js-yaml');
fs   = require('fs');

exports._safeDump = function(o){
    try {
        var d = yaml.safeDump(o);
        return { success: true, value: d, error: null };
    } catch (e) {
        return { success: false, value: "", error: e.toString() };
    }
};
