(function () {
  function createNSObject(className, interfaceType) {
    return Components.classes[className].createInstance(interfaceType);
  }

  function toHexString(charCode) {
    return ("0" + charCode.toString(16)).slice(-2);
  }

  function md5sum(string) {
    var converter = createNSObject('@mozilla.org/intl/scriptableunicodeconverter',
                                   Components.interfaces.nsIScriptableUnicodeConverter);
    converter.charset = 'UTF-8';
    var result = {};
    var data = converter.convertToByteArray(string, result);
    var ch = createNSObject('@mozilla.org/security/hash;1',
                            Components.interfaces.nsICryptoHash);
    ch.init(ch.MD5);
    ch.update(data, data.length);
    var hash = ch.finish(false);

    return [toHexString(hash.charCodeAt(i)) for (i in hash)].join("");
  }

  function TemporaryFile(textarea) {
    var f = createNSObject("@mozilla.org/file/directory_service;1",
                           Components.interfaces.nsIProperties).
              get("TmpD", Components.interfaces.nsIFile);
    f.append('textarea.' +
             md5sum(textarea.ownerDocument.URL + ':' + textarea.getAttribute('name')) +
             '.txt');
    var file = io.getFile(f.path);

    this.remove = function() {
      try {
        file.remove(false);
      } catch (e) {
      }
    };

    this.path = file.path;

    this.write = function(contents) {
      file.createUnique(Components.interfaces.nsIFile.NORMAL_FILE_TYPE, 0600);
      io.writeFile(file, contents, io.MODE_WRONLY | io.MODE_TRUNCATE, 0600);
    };

    this.accessible = function() {
      return file.exists() && file.isFile() && file.isReadable();
    };

    this.read = function() {
      return io.readFile(file);
    };

    return null;
  }

  editor.editFieldExternally = function(forceEditing) {
    if (!options["editor"])
      return;

    let textBox = null;
    if (!(config.isComposeWindow))
      textBox = liberator.focus;

    if (!textBox)
      return;

    var file = new TemporaryFile(textBox);
    if (file.accessible()) {
      try {
        textBox.value = file.read();
      } catch (e) {
        liberator.echoerr(e);
      } finally {
        file.remove();
        return;
      }
    }

    if (!forceEditing && (!file || !file.accessible()) && textBox.type == "password") {
      commandline.input("Editing a password field externally will reveal the password. Would you like to continue? (yes/[no]): ",
                        function(resp) {
                          if (resp && resp.match(/^y(es)?$/i))
                            editor.editFieldExternally(true);
                        });
      return;
    }

    var text = textBox.value;

    var oldBg = textBox.style.backgroundColor;
    var tmpBg = "yellow";
    textBox.style.backgroundColor = "#bbbbbb";
    try {
      file.write(text);
      try {
        editor.editFileExternally(file.path);
      } catch (e) {
        file.remove();
        throw e;
      }
    } catch (e) {
      liberator.echoerr(e);
      tmpBg = "red";
    }

    let colors = [tmpBg, oldBg, tmpBg, oldBg];
    (function () {
      textBox.style.backgroundColor = colors.shift();
      if (colors.length > 0)
        setTimeout(arguments.callee, 100);
    })();
  };
})();
