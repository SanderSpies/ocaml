// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2014 Jérôme Vouillon, Hugo Heuzard
// Laboratoire PPS - CNRS Université Paris Diderot
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

///////////// Io

var caml_global_data = [0];

//Provides: caml_sys_close
//Requires: caml_global_data
function caml_sys_close(fd) {
  delete caml_global_data.fds[fd];
  return 0;
}

//Provides: caml_std_output
//Requires: caml_new_string, caml_ml_string_length, caml_ml_channels
function caml_std_output(chanid, s) {
  var chan = caml_ml_channels[chanid];
  var str = caml_new_string(s);
  var slen = caml_ml_string_length(str);
  chan.file.write(chan.offset, str, 0, slen);
  chan.offset += slen;
  return 0;
}

//Provides: caml_sys_open
//Requires: caml_raise_sys_error, caml_global_data
//Requires: caml_create_bytes,MlFakeFile
//Requires: js_print_stderr, js_print_stdout
//Requires: caml_std_output
//Requires: resolve_fs_device
function caml_sys_open_internal(idx, output, file, flags) {
  if (caml_global_data.fds === undefined) caml_global_data.fds = new Array();
  flags = flags ? flags : {};
  var info = {};
  info.file = file;
  info.offset = flags.append ? file.length() : 0;
  info.flags = flags;
  info.output = output;
  caml_global_data.fds[idx] = info;
  if (!caml_global_data.fd_last_idx || idx > caml_global_data.fd_last_idx)
    caml_global_data.fd_last_idx = idx;
  return idx;
}
function caml_sys_open(name, flags, _perms) {
  var f = {};
  while (flags) {
    switch (flags[1]) {
      case 0:
        f.rdonly = 1;
        break;
      case 1:
        f.wronly = 1;
        break;
      case 2:
        f.append = 1;
        break;
      case 3:
        f.create = 1;
        break;
      case 4:
        f.truncate = 1;
        break;
      case 5:
        f.excl = 1;
        break;
      case 6:
        f.binary = 1;
        break;
      case 7:
        f.text = 1;
        break;
      case 8:
        f.nonblock = 1;
        break;
    }
    flags = flags[2];
  }
  if (f.rdonly && f.wronly)
    caml_raise_sys_error(
      name.toString() +
        " : flags Open_rdonly and Open_wronly are not compatible"
    );
  if (f.text && f.binary)
    caml_raise_sys_error(
      name.toString() + " : flags Open_text and Open_binary are not compatible"
    );
  var root = resolve_fs_device(name);
  var file = root.device.open(root.rest, f);
  var idx = caml_global_data.fd_last_idx ? caml_global_data.fd_last_idx : 0;
  return caml_sys_open_internal(idx + 1, caml_std_output, file, f);
}
caml_sys_open_internal(
  0,
  caml_std_output,
  new MlFakeFile(caml_create_bytes(0))
); //stdin
caml_sys_open_internal(
  1,
  js_print_stdout,
  new MlFakeFile(caml_create_bytes(0))
); //stdout
caml_sys_open_internal(
  2,
  js_print_stderr,
  new MlFakeFile(caml_create_bytes(0))
); //stderr

// ocaml Channels

//Provides: caml_ml_set_channel_name
function caml_ml_set_channel_name() {
  return 0;
}

//Provides: caml_ml_channels
var caml_ml_channels = new Array();

//Provides: caml_ml_out_channels_list
//Requires: caml_ml_channels
function caml_ml_out_channels_list() {
  var l = 0;
  for (var c = 0; c < caml_ml_channels.length; c++) {
    if (
      caml_ml_channels[c] &&
      caml_ml_channels[c].opened &&
      caml_ml_channels[c].out
    )
      l = [0, caml_ml_channels[c].fd, l];
  }
  return l;
}

//Provides: caml_ml_open_descriptor_out
//Requires: caml_ml_channels, caml_global_data
//Requires: caml_raise_sys_error
function caml_ml_open_descriptor_out(fd) {
  var data = caml_global_data.fds[fd];
  if (data.flags.rdonly) caml_raise_sys_error("fd " + fd + " is readonly");
  var channel = {
    file: data.file,
    offset: data.offset,
    fd: fd,
    opened: true,
    out: true,
    buffer: ""
  };
  caml_ml_channels[channel.fd] = channel;
  return channel.fd;
}

//Provides: caml_ml_open_descriptor_in
//Requires: caml_global_data,caml_sys_open,caml_raise_sys_error, caml_ml_channels
function caml_ml_open_descriptor_in(fd) {
  var data = caml_global_data.fds[fd];
  if (data.flags.wronly) caml_raise_sys_error("fd " + fd + " is writeonly");

  var channel = {
    file: data.file,
    offset: data.offset,
    fd: fd,
    opened: true,
    out: false,
    refill: null
  };
  caml_ml_channels[channel.fd] = channel;
  return channel.fd;
}

//Provides: caml_ml_set_binary_mode
//Requires: caml_global_data, caml_ml_channels
function caml_ml_set_binary_mode(chanid, mode) {
  var chan = caml_ml_channels[chanid];
  var data = caml_global_data.fds[chan.fd];
  data.flags.text = !mode;
  data.flags.binary = mode;
  return 0;
}

//Input from in_channel

//Provides: caml_ml_close_channel
//Requires: caml_ml_flush, caml_ml_channels
//Requires: caml_sys_close
function caml_ml_close_channel(chanid) {
  var chan = caml_ml_channels[chanid];
  caml_ml_flush(chanid);
  chan.opened = false;
  chan.file.close();
  caml_sys_close(chan.fd);
  return 0;
}

//Provides: caml_ml_channel_size
//Requires: caml_ml_channels
function caml_ml_channel_size(chanid) {
  var chan = caml_ml_channels[chanid];
  return chan.file.length();
}

//Provides: caml_ml_channel_size_64
//Requires: caml_int64_of_float,caml_ml_channels
function caml_ml_channel_size_64(chanid) {
  var chan = caml_ml_channels[chanid];
  return caml_int64_of_float(chan.file.length());
}

//Provides: caml_ml_set_channel_output
//Requires: caml_ml_channels, caml_global_data
function caml_ml_set_channel_output(chanid, f) {
  var chan = caml_ml_channels[chanid];
  caml_global_data.fds[chan.fd].output = f;
  return 0;
}

//Provides: caml_ml_set_channel_refill
//Requires: caml_ml_channels, caml_global_data
function caml_ml_set_channel_refill(chanid, f) {
  caml_ml_channels[chanid].refill = f;
  return 0;
}

//Provides: caml_ml_refill_input
//Requires: caml_ml_bytes_length
function caml_ml_refill_input(chan) {
  var str = chan.refill();
  var str_len = caml_ml_bytes_length(str);
  if (str_len == 0) chan.refill = null;
  chan.file.write(chan.file.length(), str, 0, str_len);
  return str_len;
}

//Provides: caml_ml_may_refill_input
//Requires: caml_ml_refill_input, caml_ml_channels
function caml_ml_may_refill_input(chanid) {
  var chan = caml_ml_channels[chanid];
  if (chan.refill == null) return;
  if (chan.file.length() != chan.offset) return;
  caml_ml_refill_input(chan);
}

//Provides: caml_ml_input
//Requires: caml_ml_refill_input, caml_ml_channels
function caml_ml_input(chanid, s, i, l) {
  var chan = caml_ml_channels[chanid];
  var l2 = chan.file.length() - chan.offset;
  if (l2 == 0 && chan.refill != null) l2 = caml_ml_refill_input(chan);
  if (l2 < l) l = l2;
  chan.file.read(chan.offset, s, i, l);
  chan.offset += l;
  return l;
}

//Provides: caml_input_value
//Requires: caml_marshal_data_size, caml_input_value_from_string, caml_create_bytes, caml_ml_channels
function caml_input_value(chanid) {
  var chan = caml_ml_channels[chanid];

  var buf = caml_create_bytes(8);
  chan.file.read(chan.offset, buf, 0, 8);

  // Header is 20 bytes
  var len = caml_marshal_data_size(buf, 0) + 20;

  var buf = caml_create_bytes(len);
  chan.file.read(chan.offset, buf, 0, len);

  var offset = [0];
  var res = caml_input_value_from_string(buf, offset);
  chan.offset = chan.offset + offset[0];
  return res;
}

//Provides: caml_ml_input_char
//Requires: caml_raise_end_of_file, caml_array_bound_error
//Requires: caml_ml_may_refill_input, caml_ml_channels
function caml_ml_input_char(chanid) {
  var chan = caml_ml_channels[chanid];
  caml_ml_may_refill_input(chanid);
  if (chan.offset >= chan.file.length()) caml_raise_end_of_file();
  var res = chan.file.read_one(chan.offset);
  chan.offset++;
  return res;
}

//Provides: caml_ml_input_int
//Requires: caml_raise_end_of_file
//Requires: caml_ml_refill_input, caml_ml_channels
function caml_ml_input_int(chanid) {
  var chan = caml_ml_channels[chanid];
  var file = chan.file;
  while (chan.offset + 3 >= file.length()) {
    var l = caml_ml_refill_input(chan);
    if (l == 0) caml_raise_end_of_file();
  }
  var o = chan.offset;
  var r =
    (file.read_one(o) << 24) |
    (file.read_one(o + 1) << 16) |
    (file.read_one(o + 2) << 8) |
    file.read_one(o + 3);
  chan.offset += 4;
  return r;
}

//Provides: caml_ml_seek_in
//Requires: caml_raise_sys_error, caml_ml_channels
function caml_ml_seek_in(chanid, pos) {
  var chan = caml_ml_channels[chanid];
  if (chan.refill != null) caml_raise_sys_error("Illegal seek");
  chan.offset = pos;
  return 0;
}

//Provides: caml_ml_seek_in_64
//Requires: caml_int64_to_float, caml_raise_sys_error, caml_ml_channels
function caml_ml_seek_in_64(chanid, pos) {
  var chan = caml_ml_channels[chanid];
  if (chan.refill != null) caml_raise_sys_error("Illegal seek");
  chan.offset = caml_int64_to_float(pos);
  return 0;
}

//Provides: caml_ml_pos_in
//Requires: caml_ml_channels
function caml_ml_pos_in(chanid) {
  return caml_ml_channels[chanid].offset;
}

//Provides: caml_ml_pos_in_64
//Requires: caml_int64_of_float, caml_ml_channels
function caml_ml_pos_in_64(chanid) {
  return caml_int64_of_float(caml_ml_channels[chanid].offset);
}

//Provides: caml_ml_input_scan_line
//Requires: caml_array_bound_error
//Requires: caml_ml_may_refill_input, caml_ml_channels
function caml_ml_input_scan_line(chanid) {
  var chan = caml_ml_channels[chanid];
  caml_ml_may_refill_input(chanid);
  var p = chan.offset;
  var len = chan.file.length();
  if (p >= len) {
    return 0;
  }
  while (true) {
    if (p >= len) return -(p - chan.offset);
    if (chan.file.read_one(p) == 10) return p - chan.offset + 1;
    p++;
  }
}

//Provides: caml_ml_flush
//Requires: caml_raise_sys_error, caml_global_data, caml_ml_channels
function caml_ml_flush(chanid) {
  var chan = caml_ml_channels[chanid];
  if (!chan.opened) caml_raise_sys_error("Cannot flush a closed channel");
  if (!chan.buffer || chan.buffer == "") return 0;
  if (
    chan.fd &&
    caml_global_data.fds[chan.fd] &&
    caml_global_data.fds[chan.fd].output
  ) {
    var output = caml_global_data.fds[chan.fd].output;
    switch (output.length) {
      case 2:
        output(chanid, chan.buffer);
        break;
      default:
        output(chan.buffer);
    }
  }
  chan.buffer = "";
  return 0;
}

//output to out_channel

//Provides: caml_ml_output_bytes
//Requires: caml_ml_flush,caml_ml_bytes_length
//Requires: caml_create_bytes, caml_blit_bytes, caml_raise_sys_error, caml_ml_channels, caml_jsbytes_of_string
function caml_ml_output_bytes(chanid, buffer, offset, len) {
  var chan = caml_ml_channels[chanid];
  if (!chan.opened) caml_raise_sys_error("Cannot output to a closed channel");
  var string;
  if (offset == 0 && caml_ml_bytes_length(buffer) == len) string = buffer;
  else {
    string = caml_create_bytes(len);
    caml_blit_bytes(buffer, offset, string, 0, len);
  }
  var jsstring = caml_jsbytes_of_string(string);
  var id = jsstring.lastIndexOf("\n");
  if (id < 0) chan.buffer += jsstring;
  else {
    chan.buffer += jsstring.substr(0, id + 1);
    caml_ml_flush(chanid);
    chan.buffer += jsstring.substr(id + 1);
  }
  return 0;
}

//Provides: caml_ml_output
//Requires: caml_ml_output_bytes
function caml_ml_output(chanid, buffer, offset, len) {
  return caml_ml_output_bytes(chanid, buffer, offset, len);
}

//Provides: caml_ml_output_char
//Requires: caml_ml_output
//Requires: caml_new_string
function caml_ml_output_char(chanid, c) {
  var s = caml_new_string(String.fromCharCode(c));
  caml_ml_output(chanid, s, 0, 1);
  return 0;
}

//Provides: caml_output_value
//Requires: caml_output_value_to_string, caml_ml_output,caml_ml_string_length
function caml_output_value(chanid, v, _flags) {
  var s = caml_output_value_to_string(v);
  caml_ml_output(chanid, s, 0, caml_ml_string_length(s));
  return 0;
}

//Provides: caml_ml_seek_out
//Requires: caml_ml_channels
function caml_ml_seek_out(chanid, pos) {
  caml_ml_channels[chanid].offset = pos;
  return 0;
}

//Provides: caml_ml_seek_out_64
//Requires: caml_int64_to_float, caml_ml_channels
function caml_ml_seek_out_64(chanid, pos) {
  caml_ml_channels[chanid].offset = caml_int64_to_float(pos);
  return 0;
}

//Provides: caml_ml_pos_out
//Requires: caml_ml_channels
function caml_ml_pos_out(chanid) {
  return caml_ml_channels[chanid].offset;
}

//Provides: caml_ml_pos_out_64
//Requires: caml_int64_of_float, caml_ml_channels
function caml_ml_pos_out_64(chanid) {
  return caml_int64_of_float(caml_ml_channels[chanid].offset);
}

//Provides: caml_ml_output_int
//Requires: caml_ml_output
//Requires: caml_string_of_array
function caml_ml_output_int(chanid, i) {
  var arr = [(i >> 24) & 0xff, (i >> 16) & 0xff, (i >> 8) & 0xff, i & 0xff];
  var s = caml_string_of_array(arr);
  caml_ml_output(chanid, s, 0, 4);
  return 0;
}

// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2014 Jérôme Vouillon, Hugo Heuzard
// Laboratoire PPS - CNRS Université Paris Diderot
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

///////////// Dummy filesystem
var joo_global_object = window;
//Provides: caml_current_dir
if (joo_global_object.process && joo_global_object.process.cwd)
  var caml_current_dir = joo_global_object.process.cwd().replace(/\\/g, "/");
else var caml_current_dir = "/static";
if (caml_current_dir.slice(-1) !== "/") caml_current_dir += "/";

//Provides: caml_root
//Requires: caml_current_dir
var caml_root = caml_current_dir.match(/[^\/]*\//)[0];

//Provides: MlFile
function MlFile() {}

//Provides: caml_make_path
//Requires: caml_current_dir,MlBytes
function caml_make_path(name) {
  name = name instanceof MlBytes ? name.toString() : name;
  if (name.charCodeAt(0) != 47) name = caml_current_dir + name;
  var comp = name.split("/");
  var ncomp = [];
  for (var i = 0; i < comp.length; i++) {
    switch (comp[i]) {
      case "..":
        if (ncomp.length > 1) ncomp.pop();
        break;
      case ".":
        break;
      case "":
        if (ncomp.length == 0) ncomp.push("");
        break;
      default:
        ncomp.push(comp[i]);
        break;
    }
  }
  ncomp.orig = name;
  return ncomp;
}

// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2014 Jérôme Vouillon, Hugo Heuzard
// Laboratoire PPS - CNRS Université Paris Diderot
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

//Provides: fs_node_supported
function fs_node_supported() {
  return (
    typeof joo_global_object.process !== "undefined" &&
    typeof joo_global_object.process.versions !== "undefined" &&
    typeof joo_global_object.process.versions.node !== "undefined"
  );
}

//Provides: MlNodeDevice
//Requires: MlNodeFile
function MlNodeDevice(root) {
  this.fs = require("fs");
  this.root = root;
}
MlNodeDevice.prototype.nm = function(name) {
  return this.root + name;
};
MlNodeDevice.prototype.exists = function(name) {
  return this.fs.existsSync(this.nm(name)) ? 1 : 0;
};
MlNodeDevice.prototype.readdir = function(name) {
  return this.fs.readdirSync(this.nm(name));
};
MlNodeDevice.prototype.is_dir = function(name) {
  return this.fs.statSync(this.nm(name)).isDirectory() ? 1 : 0;
};
MlNodeDevice.prototype.unlink = function(name) {
  var b = this.fs.existsSync(this.nm(name)) ? 1 : 0;
  this.fs.unlinkSync(this.nm(name));
  return b;
};
MlNodeDevice.prototype.open = function(name, f) {
  var consts = require("constants");
  var res = 0;
  for (var key in f) {
    switch (key) {
      case "rdonly":
        res |= consts.O_RDONLY;
        break;
      case "wronly":
        res |= consts.O_WRONLY;
        break;
      case "append":
        res |= consts.O_WRONLY | consts.O_APPEND;
        break;
      case "create":
        res |= consts.O_CREAT;
        break;
      case "truncate":
        res |= consts.O_TRUNC;
        break;
      case "excl":
        res |= consts.O_EXCL;
        break;
      case "binary":
        res |= consts.O_BINARY;
        break;
      case "text":
        res |= consts.O_TEXT;
        break;
      case "nonblock":
        res |= consts.O_NONBLOCK;
        break;
    }
  }
  var fd = this.fs.openSync(this.nm(name), res);
  return new MlNodeFile(fd);
};

MlNodeDevice.prototype.rename = function(o, n) {
  this.fs.renameSync(this.nm(o), this.nm(n));
};

MlNodeDevice.prototype.constructor = MlNodeDevice;

//Provides: MlNodeFile
//Requires: MlFile, caml_array_of_string, caml_bytes_set

var Buffer = joo_global_object.Buffer;

function MlNodeFile(fd) {
  this.fs = require("fs");
  this.fd = fd;
}
MlNodeFile.prototype = new MlFile();

MlNodeFile.prototype.truncate = function(len) {
  this.fs.ftruncateSync(this.fd, len | 0);
};
MlNodeFile.prototype.length = function() {
  return this.fs.fstatSync(this.fd).size;
};
MlNodeFile.prototype.write = function(offset, buf, buf_offset, len) {
  var a = caml_array_of_string(buf);
  if (!(a instanceof joo_global_object.Uint8Array))
    a = new joo_global_object.Uint8Array(a);
  var buffer = new Buffer(a);
  this.fs.writeSync(this.fd, buffer, buf_offset, len, offset);
  return 0;
};
MlNodeFile.prototype.read = function(offset, buf, buf_offset, len) {
  var a = caml_array_of_string(buf);
  if (!(a instanceof joo_global_object.Uint8Array))
    a = new joo_global_object.Uint8Array(a);
  var buffer = new Buffer(a);
  this.fs.readSync(this.fd, buffer, buf_offset, len, offset);
  for (var i = 0; i < len; i++) {
    caml_bytes_set(buf, buf_offset + i, buffer[buf_offset + i]);
  }
  return 0;
};
MlNodeFile.prototype.read_one = function(offset) {
  var a = new joo_global_object.Uint8Array(1);
  var buffer = new Buffer(a);
  this.fs.readSync(this.fd, buffer, 0, 1, offset);
  return buffer[0];
};
MlNodeFile.prototype.close = function() {
  this.fs.closeSync(this.fd);
};

MlNodeFile.prototype.constructor = MlNodeFile;

//Provides:jsoo_mount_point
//Requires: MlFakeDevice, MlNodeDevice, caml_root, fs_node_supported
var jsoo_mount_point = [];
if (fs_node_supported()) {
  jsoo_mount_point.push({
    path: caml_root,
    device: new MlNodeDevice(caml_root)
  });
} else {
  jsoo_mount_point.push({
    path: caml_root,
    device: new MlFakeDevice(caml_root)
  });
}
jsoo_mount_point.push({
  path: caml_root + "static/",
  device: new MlFakeDevice(caml_root + "static/")
});

//Provides:caml_list_mount_point
//Requires: jsoo_mount_point, caml_new_string
function caml_list_mount_point() {
  var prev = 0;
  for (var i = 0; i < jsoo_mount_point.length; i++) {
    var old = prev;
    prev = [0, caml_new_string(jsoo_mount_point[i].path), old];
  }
  return prev;
}

//Provides: resolve_fs_device
//Requires: caml_make_path, jsoo_mount_point
function resolve_fs_device(name) {
  var path = caml_make_path(name);
  var name = path.join("/");
  var name_slash = name + "/";
  var res;
  for (var i = 0; i < jsoo_mount_point.length; i++) {
    var m = jsoo_mount_point[i];
    if (
      name_slash.search(m.path) == 0 &&
      (!res || res.path.length < m.path.length)
    )
      res = {
        path: m.path,
        device: m.device,
        rest: name.substring(m.path.length, name.length)
      };
  }
  return res;
}

//Provides: caml_mount_autoload
//Requires: MlFakeDevice, caml_make_path, jsoo_mount_point
function caml_mount_autoload(name, f) {
  var path = caml_make_path(name);
  var name = path.join("/") + "/";
  jsoo_mount_point.push({ path: name, device: new MlFakeDevice(name, f) });
  return 0;
}

//Provides: caml_unmount
//Requires: jsoo_mount_point, caml_make_path
function caml_unmount(name) {
  var path = caml_make_path(name);
  var name = path.join("/") + "/";
  var idx = -1;
  for (var i = 0; i < jsoo_mount_point.length; i++)
    if (jsoo_mount_point[i].path == name) idx = i;
  if (idx > -1) jsoo_mount_point.splice(idx, 1);
  return 0;
}

//Provides: caml_sys_getcwd
//Requires: caml_current_dir, caml_new_string
function caml_sys_getcwd() {
  return caml_new_string(caml_current_dir);
}

//Provides: caml_sys_chdir
//Requires: caml_current_dir, caml_raise_no_such_file, resolve_fs_device
function caml_sys_chdir(dir) {
  var root = resolve_fs_device(dir);
  if (root.device.exists(root.rest)) {
    if (root.rest) caml_current_dir = root.path + root.rest + "/";
    else caml_current_dir = root.path;
    return 0;
  } else {
    caml_raise_no_such_file(dir);
  }
}

//Provides: caml_raise_no_such_file
//Requires: MlBytes, caml_raise_sys_error
function caml_raise_no_such_file(name) {
  name = name instanceof MlBytes ? name.toString() : name;
  caml_raise_sys_error(name + ": No such file or directory");
}

//Provides: caml_raise_not_a_dir
//Requires: MlBytes, caml_raise_sys_error
function caml_raise_not_a_dir(name) {
  name = name instanceof MlBytes ? name.toString() : name;
  caml_raise_sys_error(name + ": Not a directory");
}

//Provides: caml_sys_file_exists
//Requires: resolve_fs_device
function caml_sys_file_exists(name) {
  var root = resolve_fs_device(name);
  return root.device.exists(root.rest);
}

//Provides: caml_sys_read_directory
//Requires: caml_new_string
//Requires: caml_raise_not_a_dir, resolve_fs_device
function caml_sys_read_directory(name) {
  var root = resolve_fs_device(name);
  var a = root.device.readdir(root.rest);
  var l = new Array(a.length + 1);
  l[0] = 0;
  for (var i = 0; i < a.length; i++) l[i + 1] = caml_new_string(a[i]);
  return l;
}

//Provides: caml_sys_remove
//Requires: caml_raise_no_such_file, resolve_fs_device
function caml_sys_remove(name) {
  var root = resolve_fs_device(name);
  var ok = root.device.unlink(root.rest);
  if (ok == 0) caml_raise_no_such_file(name);
  return 0;
}

//Provides: caml_sys_is_directory
//Requires: resolve_fs_device
function caml_sys_is_directory(name) {
  var root = resolve_fs_device(name);
  var a = root.device.is_dir(root.rest);
  return a ? 1 : 0;
}

//Provides: caml_sys_rename
//Requires: caml_failwith, resolve_fs_device
function caml_sys_rename(o, n) {
  var o_root = resolve_fs_device(o);
  var n_root = resolve_fs_device(n);
  if (o_root.device != n_root.device)
    caml_failwith("caml_sys_rename: cannot move file between two filesystem");
  if (!o_root.device.rename) caml_failwith("caml_sys_rename: no implemented");
  o_root.device.rename(o_root.rest, n_root.rest);
}

//Provides: caml_ba_map_file
//Requires: caml_failwith
function caml_ba_map_file(vfd, kind, layout, shared, dims, pos) {
  // var data = caml_global_data.fds[vfd];
  caml_failwith("caml_ba_map_file not implemented");
}

//Provides: caml_ba_map_file_bytecode
//Requires: caml_ba_map_file
function caml_ba_map_file_bytecode(argv, argn) {
  return caml_ba_map_file(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

//Provides: caml_create_file_extern
function caml_create_file_extern(name, content) {
  if (joo_global_object.caml_create_file)
    joo_global_object.caml_create_file(name, content);
  else {
    if (!joo_global_object.caml_fs_tmp) joo_global_object.caml_fs_tmp = [];
    joo_global_object.caml_fs_tmp.push({ name: name, content: content });
  }
  return 0;
}

//Provides: caml_fs_init
//Requires: caml_create_file
function caml_fs_init() {
  var tmp = joo_global_object.caml_fs_tmp;
  if (tmp) {
    for (var i = 0; i < tmp.length; i++) {
      caml_create_file(tmp[i].name, tmp[i].content);
    }
  }
  joo_global_object.caml_create_file = caml_create_file;
  return 0;
}

//Provides: caml_create_file
//Requires: caml_failwith, resolve_fs_device
function caml_create_file(name, content) {
  var root = resolve_fs_device(name);
  if (!root.device.register) caml_failwith("cannot register file");
  root.device.register(root.rest, content);
  return 0;
}

//Provides: caml_read_file_content
//Requires: resolve_fs_device, caml_raise_no_such_file, caml_create_bytes
function caml_read_file_content(name) {
  var root = resolve_fs_device(name);
  if (root.device.exists(root.rest)) {
    var file = root.device.open(root.rest, { rdonly: 1 });
    var len = file.length();
    var buf = caml_create_bytes(len);
    file.read(0, buf, 0, len);
    return buf;
  }
  caml_raise_no_such_file(name);
}

// Js_of_ocaml library
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2010 Jérôme Vouillon
// Laboratoire PPS - CNRS Université Paris Diderot
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

///////////// Jslib

//Provides: caml_js_pure_expr const
function caml_js_pure_expr(f) {
  return f();
}

//Provides: caml_js_set (mutable, const, const)
function caml_js_set(o, f, v) {
  o[f] = v;
  return 0;
}
//Provides: caml_js_get mutable (const, const)
function caml_js_get(o, f) {
  return o[f];
}
//Provides: caml_js_delete (mutable, const)
function caml_js_delete(o, f) {
  delete o[f];
  return 0;
}

//Provides: caml_js_instanceof (const, const)
function caml_js_instanceof(o, c) {
  return o instanceof c;
}

//Provides: caml_js_typeof (const)
function caml_js_typeof(o) {
  return typeof o;
}

//Provides: caml_js_on_ie const
function caml_js_on_ie() {
  var ua = joo_global_object.navigator
    ? joo_global_object.navigator.userAgent
    : "";
  return ua.indexOf("MSIE") != -1 && ua.indexOf("Opera") != 0;
}

//Provides: caml_js_html_escape const (const)
var caml_js_regexps = { amp: /&/g, lt: /</g, quot: /\"/g, all: /[&<\"]/ };
function caml_js_html_escape(s) {
  if (!caml_js_regexps.all.test(s)) return s;
  return s
    .replace(caml_js_regexps.amp, "&amp;")
    .replace(caml_js_regexps.lt, "&lt;")
    .replace(caml_js_regexps.quot, "&quot;");
}

//Provides: caml_js_html_entities const (const)
function caml_js_html_entities(s) {
  var str,
    temp = document.createElement("p");
  temp.innerHTML = s;
  str = temp.textContent || temp.innerText;
  temp = null;
  return str;
}

/////////// Debugging console
//Provides: caml_js_get_console const
function caml_js_get_console() {
  var c = joo_global_object.console ? joo_global_object.console : {};
  var m = [
    "log",
    "debug",
    "info",
    "warn",
    "error",
    "assert",
    "dir",
    "dirxml",
    "trace",
    "group",
    "groupCollapsed",
    "groupEnd",
    "time",
    "timeEnd"
  ];
  function f() {}
  for (var i = 0; i < m.length; i++) if (!c[m[i]]) c[m[i]] = f;
  return c;
}

//Provides:caml_trampoline
function caml_trampoline(res) {
  var c = 1;
  while (res && res.joo_tramp) {
    res = res.joo_tramp.apply(null, res.joo_args);
    c++;
  }
  return res;
}

//Provides:caml_trampoline_return
function caml_trampoline_return(f, args) {
  return { joo_tramp: f, joo_args: args };
}

//Provides: js_print_stdout (const)
function js_print_stdout(s) {
  var g = joo_global_object;
  if (g.process && g.process.stdout && g.process.stdout.write) {
    g.process.stdout.write(s);
  } else {
    // Do not output the last \n if present
    // as console logging display a newline at the end
    if (s.charCodeAt(s.length - 1) == 10) s = s.substr(0, s.length - 1);
    var v = g.console;
    v && v.log && v.log(s);
  }
}
//Provides: js_print_stderr (const)
function js_print_stderr(s) {
  var g = joo_global_object;
  if (g.process && g.process.stdout && g.process.stdout.write) {
    g.process.stderr.write(s);
  } else {
    // Do not output the last \n if present
    // as console logging display a newline at the end
    if (s.charCodeAt(s.length - 1) == 10) s = s.substr(0, s.length - 1);
    var v = g.console;
    v && v.error && v.error(s);
  }
}

// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2010-2014 Jérôme Vouillon
// Laboratoire PPS - CNRS Université Paris Diderot
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

// An OCaml string is an object with three fields:
// - tag 't'
// - length 'l'
// - contents 'c'
//
// The contents of the string can be either a JavaScript array or
// a JavaScript string. The length of this string can be less than the
// length of the OCaml string. In this case, remaining bytes are
// assumed to be zeroes. Arrays are mutable but consumes more memory
// than strings. A common pattern is to start from an empty string and
// progressively fill it from the start. Partial strings makes it
// possible to implement this efficiently.
//
// When converting to and from UTF-16, we keep track of whether the
// string is composed only of ASCII characters (in which case, no
// conversion needs to be performed) or not.
//
// The string tag can thus take the following values:
//   full string     BYTE | UNKNOWN:      0
//                   BYTE | ASCII:        9
//                   BYTE | NOT_ASCII:    8
//   string prefix   PARTIAL:             2
//   array           ARRAY:               4
//
// One can use bit masking to discriminate these different cases:
//   known_encoding(x) = x&8
//   is_ascii(x) =       x&1
//   kind(x) =           x&6

//Provides: caml_str_repeat
function caml_str_repeat(n, s) {
  if (s.repeat) return s.repeat(n); // ECMAscript 6 and Firefox 24+
  var r = "",
    l = 0;
  if (n == 0) return r;
  for (;;) {
    if (n & 1) r += s;
    n >>= 1;
    if (n == 0) return r;
    s += s;
    l++;
    if (l == 9) {
      s.slice(0, 1); // flatten the string
      // then, the flattening of the whole string will be faster,
      // as it will be composed of larger pieces
    }
  }
}

//Provides: caml_subarray_to_string
//Requires: raw_array_sub
//Weakdef
// Pre ECMAScript 5, [apply] would not support array-like object.
// In such setup, Typed_array would be implemented as polyfill, and [f.apply] would
// fail here. Mark the primitive as Weakdef, so that people can override it easily.
function caml_subarray_to_string(a, i, len) {
  var f = String.fromCharCode;
  if (i == 0 && len <= 4096 && len == a.length) return f.apply(null, a);
  var s = "";
  for (; 0 < len; i += 1024, len -= 1024)
    s += f.apply(null, raw_array_sub(a, i, Math.min(len, 1024)));
  return s;
}

//Provides: caml_utf8_of_utf16
function caml_utf8_of_utf16(s) {
  for (var b = "", t = b, c, d, i = 0, l = s.length; i < l; i++) {
    c = s.charCodeAt(i);
    if (c < 0x80) {
      for (var j = i + 1; j < l && (c = s.charCodeAt(j)) < 0x80; j++);
      if (j - i > 512) {
        t.substr(0, 1);
        b += t;
        t = "";
        b += s.slice(i, j);
      } else t += s.slice(i, j);
      if (j == l) break;
      i = j;
    }
    if (c < 0x800) {
      t += String.fromCharCode(0xc0 | (c >> 6));
      t += String.fromCharCode(0x80 | (c & 0x3f));
    } else if (c < 0xd800 || c >= 0xdfff) {
      t += String.fromCharCode(
        0xe0 | (c >> 12),
        0x80 | ((c >> 6) & 0x3f),
        0x80 | (c & 0x3f)
      );
    } else if (
      c >= 0xdbff ||
      i + 1 == l ||
      (d = s.charCodeAt(i + 1)) < 0xdc00 ||
      d > 0xdfff
    ) {
      // Unmatched surrogate pair, replaced by \ufffd (replacement character)
      t += "\xef\xbf\xbd";
    } else {
      i++;
      c = (c << 10) + d - 0x35fdc00;
      t += String.fromCharCode(
        0xf0 | (c >> 18),
        0x80 | ((c >> 12) & 0x3f),
        0x80 | ((c >> 6) & 0x3f),
        0x80 | (c & 0x3f)
      );
    }
    if (t.length > 1024) {
      t.substr(0, 1);
      b += t;
      t = "";
    }
  }
  return b + t;
}

//Provides: caml_utf16_of_utf8
function caml_utf16_of_utf8(s) {
  for (var b = "", t = "", c, c1, c2, v, i = 0, l = s.length; i < l; i++) {
    c1 = s.charCodeAt(i);
    if (c1 < 0x80) {
      for (var j = i + 1; j < l && (c1 = s.charCodeAt(j)) < 0x80; j++);
      if (j - i > 512) {
        t.substr(0, 1);
        b += t;
        t = "";
        b += s.slice(i, j);
      } else t += s.slice(i, j);
      if (j == l) break;
      i = j;
    }
    v = 1;
    if (++i < l && ((c2 = s.charCodeAt(i)) & -64) == 128) {
      c = c2 + (c1 << 6);
      if (c1 < 0xe0) {
        v = c - 0x3080;
        if (v < 0x80) v = 1;
      } else {
        v = 2;
        if (++i < l && ((c2 = s.charCodeAt(i)) & -64) == 128) {
          c = c2 + (c << 6);
          if (c1 < 0xf0) {
            v = c - 0xe2080;
            if (v < 0x800 || (v >= 0xd7ff && v < 0xe000)) v = 2;
          } else {
            v = 3;
            if (++i < l && ((c2 = s.charCodeAt(i)) & -64) == 128 && c1 < 0xf5) {
              v = c2 - 0x3c82080 + (c << 6);
              if (v < 0x10000 || v > 0x10ffff) v = 3;
            }
          }
        }
      }
    }
    if (v < 4) {
      // Invalid sequence
      i -= v;
      t += "\ufffd";
    } else if (v > 0xffff)
      t += String.fromCharCode(0xd7c0 + (v >> 10), 0xdc00 + (v & 0x3ff));
    else t += String.fromCharCode(v);
    if (t.length > 1024) {
      t.substr(0, 1);
      b += t;
      t = "";
    }
  }
  return b + t;
}

//Provides: caml_is_ascii
function caml_is_ascii(s) {
  // The regular expression gets better at around this point for all browsers
  if (s.length < 24) {
    // Spidermonkey gets much slower when s.length >= 24 (on 64 bit archs)
    for (var i = 0; i < s.length; i++) if (s.charCodeAt(i) > 127) return false;
    return true;
  } else return !/[^\x00-\x7f]/.test(s);
}

//Provides: caml_to_js_string
//Requires: caml_convert_string_to_bytes, caml_is_ascii, caml_utf16_of_utf8
function caml_to_js_string(s) {
  switch (s.t) {
    case 9 /*BYTES | ASCII*/:
      return s.c;
    default:
      caml_convert_string_to_bytes(s);
    case 0 /*BYTES | UNKOWN*/:
      if (caml_is_ascii(s.c)) {
        s.t = 9; /*BYTES | ASCII*/
        return s.c;
      }
      s.t = 8; /*BYTES | NOT_ASCII*/
    case 8 /*BYTES | NOT_ASCII*/:
      return caml_utf16_of_utf8(s.c);
  }
}

//Provides: caml_string_unsafe_get mutable
function caml_string_unsafe_get(s, i) {
  switch (s.t & 6) {
    default:
      /* PARTIAL */
      if (i >= s.c.length) return 0;
    case 0 /* BYTES */:
      return s.c.charCodeAt(i);
    case 4 /* ARRAY */:
      return s.c[i];
  }
}

//Provides: caml_bytes_unsafe_get mutable
function caml_bytes_unsafe_get(s, i) {
  switch (s.t & 6) {
    default:
      /* PARTIAL */
      if (i >= s.c.length) return 0;
    case 0 /* BYTES */:
      return s.c.charCodeAt(i);
    case 4 /* ARRAY */:
      return s.c[i];
  }
}

//Provides: caml_bytes_unsafe_set
//Requires: caml_convert_string_to_array
function caml_bytes_unsafe_set(s, i, c) {
  // The OCaml compiler uses Char.unsafe_chr on integers larger than 255!
  c &= 0xff;
  if (s.t != 4 /* ARRAY */) {
    if (i == s.c.length) {
      s.c += String.fromCharCode(c);
      if (i + 1 == s.l) s.t = 0; /*BYTES | UNKOWN*/
      return 0;
    }
    caml_convert_string_to_array(s);
  }
  s.c[i] = c;
  return 0;
}

//Provides: caml_string_unsafe_set
//Requires: caml_bytes_unsafe_set
function caml_string_unsafe_set(s, i, c) {
  return caml_bytes_unsafe_set(s, i, c);
}

//Provides: caml_string_bound_error
//Requires: caml_invalid_argument
function caml_string_bound_error() {
  caml_invalid_argument("index out of bounds");
}

//Provides: caml_string_get
//Requires: caml_string_bound_error, caml_string_unsafe_get
function caml_string_get(s, i) {
  if (i >>> 0 >= s.l) caml_string_bound_error();
  return caml_string_unsafe_get(s, i);
}

//Provides: caml_string_get16
//Requires: caml_string_unsafe_get, caml_string_bound_error
function caml_string_get16(s, i) {
  if (i >>> 0 >= s.l + 1) caml_string_bound_error();
  var b1 = caml_string_unsafe_get(s, i),
    b2 = caml_string_unsafe_get(s, i + 1);
  return (b2 << 8) | b1;
}

//Provides: caml_bytes_get16
//Requires: caml_string_unsafe_get, caml_string_bound_error
function caml_bytes_get16(s, i) {
  if (i >>> 0 >= s.l + 1) caml_string_bound_error();
  var b1 = caml_string_unsafe_get(s, i),
    b2 = caml_string_unsafe_get(s, i + 1);
  return (b2 << 8) | b1;
}

//Provides: caml_string_get32
//Requires: caml_string_unsafe_get, caml_string_bound_error
function caml_string_get32(s, i) {
  if (i >>> 0 >= s.l + 3) caml_string_bound_error();
  var b1 = caml_string_unsafe_get(s, i),
    b2 = caml_string_unsafe_get(s, i + 1),
    b3 = caml_string_unsafe_get(s, i + 2),
    b4 = caml_string_unsafe_get(s, i + 3);
  return (b4 << 24) | (b3 << 16) | (b2 << 8) | b1;
}

//Provides: caml_bytes_get32
//Requires: caml_string_unsafe_get, caml_string_bound_error
function caml_bytes_get32(s, i) {
  if (i >>> 0 >= s.l + 3) caml_string_bound_error();
  var b1 = caml_string_unsafe_get(s, i),
    b2 = caml_string_unsafe_get(s, i + 1),
    b3 = caml_string_unsafe_get(s, i + 2),
    b4 = caml_string_unsafe_get(s, i + 3);
  return (b4 << 24) | (b3 << 16) | (b2 << 8) | b1;
}

//Provides: caml_string_get64
//Requires: caml_string_unsafe_get, caml_string_bound_error
//Requires: caml_int64_of_bytes
function caml_string_get64(s, i) {
  if (i >>> 0 >= s.l + 7) caml_string_bound_error();
  var a = new Array(8);
  for (var j = 0; j < 8; j++) {
    a[7 - j] = caml_string_unsafe_get(s, i + j);
  }
  return caml_int64_of_bytes(a);
}

//Provides: caml_bytes_get64
//Requires: caml_string_unsafe_get, caml_string_bound_error
//Requires: caml_int64_of_bytes
function caml_bytes_get64(s, i) {
  if (i >>> 0 >= s.l + 7) caml_string_bound_error();
  var a = new Array(8);
  for (var j = 0; j < 8; j++) {
    a[7 - j] = caml_string_unsafe_get(s, i + j);
  }
  return caml_int64_of_bytes(a);
}

//Provides: caml_bytes_get
//Requires: caml_string_bound_error, caml_bytes_unsafe_get
function caml_bytes_get(s, i) {
  if (i >>> 0 >= s.l) caml_string_bound_error();
  return caml_bytes_unsafe_get(s, i);
}

//Provides: caml_string_set
//Requires: caml_string_unsafe_set, caml_string_bound_error
function caml_string_set(s, i, c) {
  if (i >>> 0 >= s.l) caml_string_bound_error();
  return caml_string_unsafe_set(s, i, c);
}

//Provides: caml_bytes_set16
//Requires: caml_string_bound_error, caml_string_unsafe_set
function caml_bytes_set16(s, i, i16) {
  if (i >>> 0 >= s.l + 1) caml_string_bound_error();
  var b2 = 0xff & (i16 >> 8),
    b1 = 0xff & i16;
  caml_string_unsafe_set(s, i + 0, b1);
  caml_string_unsafe_set(s, i + 1, b2);
  return 0;
}

//Provides: caml_string_set16
//Requires: caml_bytes_set16
function caml_string_set16(s, i, i16) {
  return caml_bytes_set16(s, i, i16);
}

//Provides: caml_bytes_set32
//Requires: caml_string_bound_error, caml_string_unsafe_set
function caml_bytes_set32(s, i, i32) {
  if (i >>> 0 >= s.l + 3) caml_string_bound_error();
  var b4 = 0xff & (i32 >> 24),
    b3 = 0xff & (i32 >> 16),
    b2 = 0xff & (i32 >> 8),
    b1 = 0xff & i32;
  caml_string_unsafe_set(s, i + 0, b1);
  caml_string_unsafe_set(s, i + 1, b2);
  caml_string_unsafe_set(s, i + 2, b3);
  caml_string_unsafe_set(s, i + 3, b4);
  return 0;
}

//Provides: caml_string_set32
//Requires: caml_bytes_set32
function caml_string_set32(s, i, i32) {
  return caml_bytes_set32(s, i, i32);
}

//Provides: caml_bytes_set64
//Requires: caml_string_bound_error, caml_string_unsafe_set
//Requires: caml_int64_to_bytes
function caml_bytes_set64(s, i, i64) {
  if (i >>> 0 >= s.l + 7) caml_string_bound_error();
  var a = caml_int64_to_bytes(i64);
  for (var j = 0; j < 8; j++) {
    caml_string_unsafe_set(s, i + 7 - j, a[j]);
  }
  return 0;
}

//Provides: caml_string_set64
//Requires: caml_bytes_set64
function caml_string_set64(s, i, i64) {
  return caml_bytes_set64(s, i, i64);
}

//Provides: caml_bytes_set
//Requires: caml_string_bound_error, caml_bytes_unsafe_set
function caml_bytes_set(s, i, c) {
  if (i >>> 0 >= s.l) caml_string_bound_error();
  return caml_bytes_unsafe_set(s, i, c);
}

//Provides: MlBytes
//Requires: caml_to_js_string
function MlBytes(tag, contents, length) {
  this.t = tag;
  this.c = contents;
  this.l = length;
}
MlBytes.prototype.toString = function() {
  return caml_to_js_string(this);
};

//Provides: caml_convert_string_to_bytes
//Requires: caml_str_repeat, caml_subarray_to_string
function caml_convert_string_to_bytes(s) {
  /* Assumes not BYTES */
  if (s.t == 2 /* PARTIAL */) s.c += caml_str_repeat(s.l - s.c.length, "\0");
  else s.c = caml_subarray_to_string(s.c, 0, s.c.length);
  s.t = 0; /*BYTES | UNKOWN*/
}

//Provides: caml_convert_string_to_array
function caml_convert_string_to_array(s) {
  /* Assumes not ARRAY */
  if (joo_global_object.Uint8Array) {
    var a = new joo_global_object.Uint8Array(s.l);
  } else {
    var a = new Array(s.l);
  }
  var b = s.c,
    l = b.length,
    i = 0;
  for (; i < l; i++) a[i] = b.charCodeAt(i);
  for (l = s.l; i < l; i++) a[i] = 0;
  s.c = a;
  s.t = 4; /* ARRAY */
  return a;
}

//Provides: caml_array_of_string mutable
//Requires: caml_convert_string_to_array
function caml_array_of_string(s) {
  if (s.t != 4 /* ARRAY */) caml_convert_string_to_array(s);
  return s.c;
}

//Provides: caml_jsbytes_of_string mutable
//Requires: caml_convert_string_to_bytes
function caml_jsbytes_of_string(s) {
  if ((s.t & 6) != 0 /* BYTES */) caml_convert_string_to_bytes(s);
  return s.c;
}

//Provides: caml_js_to_string const
//Requires: caml_is_ascii, caml_utf8_of_utf16, MlBytes
function caml_js_to_string(s) {
  var tag = 9 /* BYTES | ASCII */;
  if (!caml_is_ascii(s))
    (tag = 8) /* BYTES | NOT_ASCII */, (s = caml_utf8_of_utf16(s));
  return new MlBytes(tag, s, s.length);
}

//Provides: caml_create_string const
//Requires: MlBytes,caml_invalid_argument
function caml_create_string(len) {
  if (len < 0) caml_invalid_argument("String.create");
  return new MlBytes(len ? 2 : 9, "", len);
}
//Provides: caml_create_bytes const
//Requires: MlBytes,caml_invalid_argument
function caml_create_bytes(len) {
  if (len < 0) caml_invalid_argument("Bytes.create");
  return new MlBytes(len ? 2 : 9, "", len);
}

//Provides: caml_new_string const (const)
//Requires: MlBytes
function caml_new_string(s) {
  console.log("Create a new string for:", s);
  const allocatedMemory = env.caml_alloc(s.length + 4);

  return new MlBytes(0, s, s.length);
}

//Provides: caml_string_of_array
//Requires: MlBytes
function caml_string_of_array(a) {
  return new MlBytes(4, a, a.length);
}

//Provides: caml_string_compare mutable
//Requires: caml_convert_string_to_bytes
function caml_string_compare(s1, s2) {
  s1.t & 6 && caml_convert_string_to_bytes(s1);
  s2.t & 6 && caml_convert_string_to_bytes(s2);
  return s1.c < s2.c ? -1 : s1.c > s2.c ? 1 : 0;
}

//Provides: caml_bytes_compare mutable
//Requires: caml_convert_string_to_bytes
function caml_bytes_compare(s1, s2) {
  s1.t & 6 && caml_convert_string_to_bytes(s1);
  s2.t & 6 && caml_convert_string_to_bytes(s2);
  return s1.c < s2.c ? -1 : s1.c > s2.c ? 1 : 0;
}

//Provides: caml_string_equal mutable (const, const)
//Requires: caml_convert_string_to_bytes
function caml_string_equal(s1, s2) {
  if (s1 === s2) return 1;
  s1.t & 6 && caml_convert_string_to_bytes(s1);
  s2.t & 6 && caml_convert_string_to_bytes(s2);
  return s1.c == s2.c ? 1 : 0;
}

//Provides: caml_bytes_equal mutable (const, const)
//Requires: caml_convert_string_to_bytes
function caml_bytes_equal(s1, s2) {
  if (s1 === s2) return 1;
  s1.t & 6 && caml_convert_string_to_bytes(s1);
  s2.t & 6 && caml_convert_string_to_bytes(s2);
  return s1.c == s2.c ? 1 : 0;
}

//Provides: caml_string_notequal mutable (const, const)
//Requires: caml_string_equal
function caml_string_notequal(s1, s2) {
  return 1 - caml_string_equal(s1, s2);
}

//Provides: caml_bytes_notequal mutable (const, const)
//Requires: caml_string_equal
function caml_bytes_notequal(s1, s2) {
  return 1 - caml_string_equal(s1, s2);
}

//Provides: caml_string_lessequal mutable
//Requires: caml_convert_string_to_bytes
function caml_string_lessequal(s1, s2) {
  s1.t & 6 && caml_convert_string_to_bytes(s1);
  s2.t & 6 && caml_convert_string_to_bytes(s2);
  return s1.c <= s2.c ? 1 : 0;
}

//Provides: caml_bytes_lessequal mutable
//Requires: caml_convert_string_to_bytes
function caml_bytes_lessequal(s1, s2) {
  s1.t & 6 && caml_convert_string_to_bytes(s1);
  s2.t & 6 && caml_convert_string_to_bytes(s2);
  return s1.c <= s2.c ? 1 : 0;
}

//Provides: caml_string_lessthan mutable
//Requires: caml_convert_string_to_bytes
function caml_string_lessthan(s1, s2) {
  s1.t & 6 && caml_convert_string_to_bytes(s1);
  s2.t & 6 && caml_convert_string_to_bytes(s2);
  return s1.c < s2.c ? 1 : 0;
}

//Provides: caml_bytes_lessthan mutable
//Requires: caml_convert_string_to_bytes
function caml_bytes_lessthan(s1, s2) {
  s1.t & 6 && caml_convert_string_to_bytes(s1);
  s2.t & 6 && caml_convert_string_to_bytes(s2);
  return s1.c < s2.c ? 1 : 0;
}

//Provides: caml_string_greaterequal
//Requires: caml_string_lessequal
function caml_string_greaterequal(s1, s2) {
  return caml_string_lessequal(s2, s1);
}
//Provides: caml_bytes_greaterequal
//Requires: caml_bytes_lessequal
function caml_bytes_greaterequal(s1, s2) {
  return caml_bytes_lessequal(s2, s1);
}

//Provides: caml_string_greaterthan
//Requires: caml_string_lessthan
function caml_string_greaterthan(s1, s2) {
  return caml_string_lessthan(s2, s1);
}

//Provides: caml_bytes_greaterthan
//Requires: caml_bytes_lessthan
function caml_bytes_greaterthan(s1, s2) {
  return caml_bytes_lessthan(s2, s1);
}

//Provides: caml_fill_bytes
//Requires: caml_str_repeat, caml_convert_string_to_array
function caml_fill_bytes(s, i, l, c) {
  if (l > 0) {
    if (i == 0 && (l >= s.l || (s.t == 2 /* PARTIAL */ && l >= s.c.length))) {
      if (c == 0) {
        s.c = "";
        s.t = 2; /* PARTIAL */
      } else {
        s.c = caml_str_repeat(l, String.fromCharCode(c));
        s.t = l == s.l ? 0 /* BYTES | UNKOWN */ : 2; /* PARTIAL */
      }
    } else {
      if (s.t != 4 /* ARRAY */) caml_convert_string_to_array(s);
      for (l += i; i < l; i++) s.c[i] = c;
    }
  }
  return 0;
}

//Provides: caml_fill_string
//Requires: caml_fill_bytes
var caml_fill_string = caml_fill_bytes;

//Provides: caml_blit_bytes
//Requires: caml_subarray_to_string, caml_convert_string_to_array
function caml_blit_bytes(s1, i1, s2, i2, len) {
  if (len == 0) return 0;
  if (
    i2 == 0 &&
    (len >= s2.l || (s2.t == 2 /* PARTIAL */ && len >= s2.c.length))
  ) {
    s2.c =
      s1.t == 4 /* ARRAY */
        ? caml_subarray_to_string(s1.c, i1, len)
        : i1 == 0 && s1.c.length == len
          ? s1.c
          : s1.c.substr(i1, len);
    s2.t = s2.c.length == s2.l ? 0 /* BYTES | UNKOWN */ : 2; /* PARTIAL */
  } else if (s2.t == 2 /* PARTIAL */ && i2 == s2.c.length) {
    s2.c +=
      s1.t == 4 /* ARRAY */
        ? caml_subarray_to_string(s1.c, i1, len)
        : i1 == 0 && s1.c.length == len
          ? s1.c
          : s1.c.substr(i1, len);
    s2.t = s2.c.length == s2.l ? 0 /* BYTES | UNKOWN */ : 2; /* PARTIAL */
  } else {
    if (s2.t != 4 /* ARRAY */) caml_convert_string_to_array(s2);
    var c1 = s1.c,
      c2 = s2.c;
    if (s1.t == 4 /* ARRAY */) {
      if (i2 <= i1) {
        for (var i = 0; i < len; i++) c2[i2 + i] = c1[i1 + i];
      } else {
        for (var i = len - 1; i >= 0; i--) c2[i2 + i] = c1[i1 + i];
      }
    } else {
      var l = Math.min(len, c1.length - i1);
      for (var i = 0; i < l; i++) c2[i2 + i] = c1.charCodeAt(i1 + i);
      for (; i < len; i++) c2[i2 + i] = 0;
    }
  }
  return 0;
}

//Provides: caml_blit_string
//Requires: caml_blit_bytes
function caml_blit_string(s1, i1, s2, i2, len) {
  // TODO: s1 -> string to bytes
  return caml_blit_bytes(s1, i1, s2, i2, len);
}

//Provides: caml_ml_string_length const
function caml_ml_string_length(s) {
  return s.l;
}

//Provides: caml_ml_bytes_length const
function caml_ml_bytes_length(s) {
  return s.l;
}

//Provides: caml_string_of_bytes const
function caml_string_of_bytes(s) {
  return s;
}

//Provides: caml_bytes_of_string const
function caml_bytes_of_string(s) {
  return s;
}

/* copied from runtime.js for now */

// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2010 Jérôme Vouillon
// Laboratoire PPS - CNRS Université Paris Diderot
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

///////////// Core

//Provides: raw_array_sub
function raw_array_sub(a, i, l) {
  var b = new Array(l);
  for (var j = 0; j < l; j++) b[j] = a[i + j];
  return b;
}

//Provides: raw_array_copy
function raw_array_copy(a) {
  var l = a.length;
  var b = new Array(l);
  for (var i = 0; i < l; i++) b[i] = a[i];
  return b;
}

//Provides: raw_array_cons
function raw_array_cons(a, x) {
  var l = a.length;
  var b = new Array(l + 1);
  b[0] = x;
  for (var i = 1; i <= l; i++) b[i] = a[i - 1];
  return b;
}

//Provides: raw_array_append_one
function raw_array_append_one(a, x) {
  var l = a.length;
  var b = new Array(l + 1);
  var i = 0;
  for (; i < l; i++) b[i] = a[i];
  b[i] = x;
  return b;
}

//Provides: caml_call_gen (const, shallow)
//Requires: raw_array_sub
//Requires: raw_array_append_one
function caml_call_gen(f, args) {
  if (f.fun) return caml_call_gen(f.fun, args);
  var n = f.length;
  var argsLen = args.length;
  var d = n - argsLen;
  if (d == 0) return f.apply(null, args);
  else if (d < 0)
    return caml_call_gen(
      f.apply(null, raw_array_sub(args, 0, n)),
      raw_array_sub(args, n, argsLen - n)
    );
  else
    return function(x) {
      return caml_call_gen(f, raw_array_append_one(args, x));
    };
}

//Provides: caml_named_values
var caml_named_values = {};

//Provides: caml_register_named_value (const,const)
//Requires: caml_named_values, caml_jsbytes_of_string
function caml_register_named_value(nm, v) {
  caml_named_values[caml_jsbytes_of_string(nm)] = v;
  return 0;
}

//Provides: caml_named_value
//Requires: caml_named_values
function caml_named_value(nm) {
  return caml_named_values[nm];
}

//Provides: caml_global_data

//Provides: caml_register_global (const, shallow, const)
//Requires: caml_global_data
function caml_register_global(n, v, name_opt) {
  if (name_opt && joo_global_object.toplevelReloc)
    n = joo_global_object.toplevelReloc(name_opt);
  caml_global_data[n + 1] = v;
  if (name_opt) caml_global_data[name_opt] = v;
}

//Provides: caml_get_global_data mutable
//Requires: caml_global_data
function caml_get_global_data() {
  return caml_global_data;
}

//Raise exception

//Provides: caml_raise_constant (const)
//Version: < 4.02
function caml_raise_constant(tag) {
  throw [0, tag];
}

//Provides: caml_raise_constant (const)
//Version: >= 4.02
function caml_raise_constant(tag) {
  throw tag;
}

//Provides: caml_return_exn_constant (const)
//Version: < 4.02
function caml_return_exn_constant(tag) {
  return [0, tag];
}

//Provides: caml_return_exn_constant (const)
//Version: >= 4.02
function caml_return_exn_constant(tag) {
  return tag;
}

//Provides: caml_raise_with_arg (const, const)
function caml_raise_with_arg(tag, arg) {
  throw [0, tag, arg];
}

//Provides: caml_raise_with_string (const, const)
//Requires: caml_raise_with_arg,caml_new_string
function caml_raise_with_string(tag, msg) {
  caml_raise_with_arg(tag, caml_new_string(msg));
}

//Provides: caml_raise_sys_error (const)
//Requires: caml_raise_with_string, caml_global_data
function caml_raise_sys_error(msg) {
  caml_raise_with_string(caml_global_data.Sys_error, msg);
}

//Provides: caml_failwith (const)
//Requires: caml_raise_with_string, caml_global_data
function caml_failwith(msg) {
  caml_raise_with_string(caml_global_data.Failure, msg);
}

//Provides: caml_wrap_exception const (const)
//Requires: caml_global_data,caml_js_to_string,caml_named_value
//Requires: caml_return_exn_constant
function caml_wrap_exception(e) {
  if (e instanceof Array) return e;
  //Stack_overflow: chrome, safari
  if (
    joo_global_object.RangeError &&
    e instanceof joo_global_object.RangeError &&
    e.message &&
    e.message.match(/maximum call stack/i)
  )
    return caml_return_exn_constant(caml_global_data.Stack_overflow);
  //Stack_overflow: firefox
  if (
    joo_global_object.InternalError &&
    e instanceof joo_global_object.InternalError &&
    e.message &&
    e.message.match(/too much recursion/i)
  )
    return caml_return_exn_constant(caml_global_data.Stack_overflow);
  //Wrap Error in Js.Error exception
  if (e instanceof joo_global_object.Error && caml_named_value("jsError"))
    return [0, caml_named_value("jsError"), e];
  //fallback: wrapped in Failure
  return [0, caml_global_data.Failure, caml_js_to_string(String(e))];
}

// Experimental
//Provides: caml_exn_with_js_backtrace
//Requires: caml_global_data
function caml_exn_with_js_backtrace(exn, force) {
  //never reraise for constant exn
  if (!exn.js_error || force || exn[0] == 248)
    exn.js_error = new joo_global_object.Error(
      "Js exception containing backtrace"
    );
  return exn;
}
//Provides: caml_js_error_of_exception
function caml_js_error_of_exception(exn) {
  if (exn.js_error) {
    return exn.js_error;
  }
  return null;
}

//Provides: caml_invalid_argument (const)
//Requires: caml_raise_with_string, caml_global_data
function caml_invalid_argument(msg) {
  caml_raise_with_string(caml_global_data.Invalid_argument, msg);
}

//Provides: caml_raise_end_of_file
//Requires: caml_raise_constant, caml_global_data
function caml_raise_end_of_file() {
  caml_raise_constant(caml_global_data.End_of_file);
}

//Provides: caml_raise_zero_divide
//Requires: caml_raise_constant, caml_global_data
function caml_raise_zero_divide() {
  caml_raise_constant(caml_global_data.Division_by_zero);
}

//Provides: caml_raise_not_found
//Requires: caml_raise_constant, caml_global_data
function caml_raise_not_found() {
  caml_raise_constant(caml_global_data.Not_found);
}

//Provides: caml_array_bound_error
//Requires: caml_invalid_argument
function caml_array_bound_error() {
  caml_invalid_argument("index out of bounds");
}

//Provides: caml_update_dummy
function caml_update_dummy(x, y) {
  if (typeof y === "function") {
    x.fun = y;
    return 0;
  }
  if (y.fun) {
    x.fun = y.fun;
    return 0;
  }
  var i = y.length;
  while (i--) x[i] = y[i];
  return 0;
}

//Provides: caml_obj_is_block const (const)
function caml_obj_is_block(x) {
  return +(x instanceof Array);
}
//Provides: caml_obj_tag const (const)
//Requires: MlBytes
function caml_obj_tag(x) {
  return x instanceof Array ? x[0] : x instanceof MlBytes ? 252 : 1000;
}
//Provides: caml_obj_set_tag (mutable, const)
function caml_obj_set_tag(x, tag) {
  x[0] = tag;
  return 0;
}
//Provides: caml_obj_block const (const,const)
function caml_obj_block(tag, size) {
  var o = new Array(size + 1);
  o[0] = tag;
  for (var i = 1; i <= size; i++) o[i] = 0;
  return o;
}
//Provides: caml_obj_dup mutable (const)
function caml_obj_dup(x) {
  var l = x.length;
  var a = new Array(l);
  for (var i = 0; i < l; i++) a[i] = x[i];
  return a;
}
//Provides: caml_obj_truncate (mutable, const)
//Requires: caml_invalid_argument
function caml_obj_truncate(x, s) {
  if (s <= 0 || s + 1 > x.length) caml_invalid_argument("Obj.truncate");
  if (x.length != s + 1) x.length = s + 1;
  return 0;
}

//Provides: caml_lazy_make_forward const (const)
function caml_lazy_make_forward(v) {
  return [250, v];
}

//Provides: caml_mul const
if (!Math.imul)
  Math.imul = function(x, y) {
    y |= 0;
    return ((((x >> 16) * y) << 16) + (x & 0xffff) * y) | 0;
  };
var caml_mul = Math.imul;

//slightly slower
// function mul32(x,y) {
//   var xlo = x & 0xffff;
//   var xhi = x - xlo;
//   return (((xhi * y) |0) + xlo * y)|0;
// }

//Provides: caml_div
//Requires: caml_raise_zero_divide
function caml_div(x, y) {
  if (y == 0) caml_raise_zero_divide();
  return (x / y) | 0;
}

//Provides: caml_mod
//Requires: caml_raise_zero_divide
function caml_mod(x, y) {
  if (y == 0) caml_raise_zero_divide();
  return x % y;
}

///////////// Pervasive
//Provides: caml_array_set (mutable, const, const)
//Requires: caml_array_bound_error
function caml_array_set(array, index, newval) {
  if (index < 0 || index >= array.length - 1) caml_array_bound_error();
  array[index + 1] = newval;
  return 0;
}

//Provides: caml_array_get mutable (const, const)
//Requires: caml_array_bound_error
function caml_array_get(array, index) {
  if (index < 0 || index >= array.length - 1) caml_array_bound_error();
  return array[index + 1];
}

//Provides: caml_check_bound (const, const)
//Requires: caml_array_bound_error
function caml_check_bound(array, index) {
  if (index >>> 0 >= array.length - 1) caml_array_bound_error();
  return array;
}

//Provides: caml_make_vect const (const, const)
function caml_make_vect(len, init) {
  var len = (len + 1) | 0;
  var b = new Array(len);
  b[0] = 0;
  for (var i = 1; i < len; i++) b[i] = init;
  return b;
}

//Provides: caml_make_float_vect const (const)
function caml_make_float_vect(len) {
  var len = (len + 1) | 0;
  var b = new Array(len);
  b[0] = 254;
  for (var i = 1; i < len; i++) b[i] = 0;
  return b;
}
//Provides: caml_floatarray_create const (const)
function caml_floatarray_create(len) {
  var len = (len + 1) | 0;
  var b = new Array(len);
  b[0] = 254;
  for (var i = 1; i < len; i++) b[i] = 0;
  return b;
}

//Provides: caml_compare_val (const, const, const)
//Requires: MlBytes, caml_int64_compare, caml_int_compare, caml_string_compare
//Requires: caml_invalid_argument
function caml_compare_val(a, b, total) {
  var stack = [];
  for (;;) {
    if (!(total && a === b)) {
      if (a instanceof MlBytes) {
        if (b instanceof MlBytes) {
          if (a !== b) {
            var x = caml_string_compare(a, b);
            if (x != 0) return x;
          }
        }
        // Should not happen
        else return 1;
      } else if (a instanceof Array && a[0] === (a[0] | 0)) {
        var ta = a[0];
        // ignore double_array_tag
        if (ta === 254) ta = 0;
        // Forward object
        if (ta === 250) {
          a = a[1];
          continue;
        } else if (b instanceof Array && b[0] === (b[0] | 0)) {
          var tb = b[0];
          // ignore double_array_tag
          if (tb === 254) tb = 0;
          // Forward object
          if (tb === 250) {
            b = b[1];
            continue;
          } else if (ta != tb) {
            return ta < tb ? -1 : 1;
          } else {
            switch (ta) {
              case 248: {
                // Object
                var x = caml_int_compare(a[2], b[2]);
                if (x != 0) return x;
                break;
              }
              case 251: {
                caml_invalid_argument("equal: abstract value");
              }
              case 255: {
                // Int64
                var x = caml_int64_compare(a, b);
                if (x != 0) return x;
                break;
              }
              default:
                if (a.length != b.length) return a.length < b.length ? -1 : 1;
                if (a.length > 1) stack.push(a, b, 1);
            }
          }
        } else return 1;
      } else if (
        b instanceof MlBytes ||
        (b instanceof Array && b[0] === (b[0] | 0))
      ) {
        return -1;
      } else if (typeof a != "number" && a && a.compare) {
        var cmp = a.compare(b, total);
        if (cmp != 0) return cmp;
      } else if (typeof a == "function") {
        caml_invalid_argument("compare: functional value");
      } else {
        if (a < b) return -1;
        if (a > b) return 1;
        if (a != b) {
          if (!total) return NaN;
          if (a == a) return 1;
          if (b == b) return -1;
        }
      }
    }
    if (stack.length == 0) return 0;
    var i = stack.pop();
    b = stack.pop();
    a = stack.pop();
    if (i + 1 < a.length) stack.push(a, b, i + 1);
    a = a[i];
    b = b[i];
  }
}
//Provides: caml_compare (const, const)
//Requires: caml_compare_val
function caml_compare(a, b) {
  return caml_compare_val(a, b, true);
}
//Provides: caml_int_compare mutable (const, const)
function caml_int_compare(a, b) {
  if (a < b) return -1;
  if (a == b) return 0;
  return 1;
}
//Provides: caml_equal mutable (const, const)
//Requires: caml_compare_val
function caml_equal(x, y) {
  return +(caml_compare_val(x, y, false) == 0);
}
//Provides: caml_notequal mutable (const, const)
//Requires: caml_compare_val
function caml_notequal(x, y) {
  return +(caml_compare_val(x, y, false) != 0);
}
//Provides: caml_greaterequal mutable (const, const)
//Requires: caml_compare_val
function caml_greaterequal(x, y) {
  return +(caml_compare_val(x, y, false) >= 0);
}
//Provides: caml_greaterthan mutable (const, const)
//Requires: caml_compare_val
function caml_greaterthan(x, y) {
  return +(caml_compare_val(x, y, false) > 0);
}
//Provides: caml_lessequal mutable (const, const)
//Requires: caml_compare_val
function caml_lessequal(x, y) {
  return +(caml_compare_val(x, y, false) <= 0);
}
//Provides: caml_lessthan mutable (const, const)
//Requires: caml_compare_val
function caml_lessthan(x, y) {
  return +(caml_compare_val(x, y, false) < 0);
}

//Provides: caml_parse_sign_and_base
//Requires: caml_string_unsafe_get, caml_ml_string_length
function caml_parse_sign_and_base(s) {
  var i = 0,
    len = caml_ml_string_length(s),
    base = 10,
    sign = 1;
  if (len > 0) {
    switch (caml_string_unsafe_get(s, i)) {
      case 45:
        i++;
        sign = -1;
        break;
      case 43:
        i++;
        sign = 1;
        break;
    }
  }
  if (i + 1 < len && caml_string_unsafe_get(s, i) == 48)
    switch (caml_string_unsafe_get(s, i + 1)) {
      case 120:
      case 88:
        base = 16;
        i += 2;
        break;
      case 111:
      case 79:
        base = 8;
        i += 2;
        break;
      case 98:
      case 66:
        base = 2;
        i += 2;
        break;
    }
  return [i, sign, base];
}

//Provides: caml_parse_digit
function caml_parse_digit(c) {
  if (c >= 48 && c <= 57) return c - 48;
  if (c >= 65 && c <= 90) return c - 55;
  if (c >= 97 && c <= 122) return c - 87;
  return -1;
}

//Provides: caml_int_of_string (const)
//Requires: caml_ml_string_length, caml_string_unsafe_get
//Requires: caml_parse_sign_and_base, caml_parse_digit, caml_failwith
function caml_int_of_string(s) {
  var r = caml_parse_sign_and_base(s);
  var i = r[0],
    sign = r[1],
    base = r[2];
  var len = caml_ml_string_length(s);
  var threshold = -1 >>> 0;
  var c = i < len ? caml_string_unsafe_get(s, i) : 0;
  var d = caml_parse_digit(c);
  if (d < 0 || d >= base) caml_failwith("int_of_string");
  var res = d;
  for (i++; i < len; i++) {
    c = caml_string_unsafe_get(s, i);
    if (c == 95) continue;
    d = caml_parse_digit(c);
    if (d < 0 || d >= base) break;
    res = base * res + d;
    if (res > threshold) caml_failwith("int_of_string");
  }
  if (i != len) caml_failwith("int_of_string");
  // For base different from 10, we expect an unsigned representation,
  // hence any value of 'res' (less than 'threshold') is acceptable.
  // But we have to convert the result back to a signed integer.
  res = sign * res;
  if (base == 10 && (res | 0) != res)
    /* Signed representation expected, allow -2^(nbits-1) to 2^(nbits-1) - 1 */
    caml_failwith("int_of_string");
  return res | 0;
}

//Provides: caml_float_of_string (const)
//Requires: caml_failwith, caml_jsbytes_of_string
function caml_float_of_string(s) {
  var res;
  s = caml_jsbytes_of_string(s);
  res = +s;
  if (s.length > 0 && res === res) return res;
  s = s.replace(/_/g, "");
  res = +s;
  if ((s.length > 0 && res === res) || /^[+-]?nan$/i.test(s)) return res;
  var m = /^ *([+-]?)0x([0-9a-f]+)\.?([0-9a-f]*)p([+-]?[0-9]+)/i.exec(s);
  //            1        2             3           4
  if (m) {
    var m3 = m[3].replace(/0+$/, "");
    var mantissa = parseInt(m[1] + m[2] + m3, 16);
    var exponent = (m[4] | 0) - 4 * m3.length;
    res = mantissa * Math.pow(2, exponent);
    return res;
  }
  if (/^\+?inf(inity)?$/i.test(s)) return Infinity;
  if (/^-inf(inity)?$/i.test(s)) return -Infinity;
  caml_failwith("float_of_string");
}

//Provides: caml_is_printable const (const)
function caml_is_printable(c) {
  return +(c > 31 && c < 127);
}

///////////// Format
//Provides: caml_parse_format
//Requires: caml_jsbytes_of_string, caml_invalid_argument
function caml_parse_format(fmt) {
  fmt = caml_jsbytes_of_string(fmt);
  var len = fmt.length;
  if (len > 31) caml_invalid_argument("format_int: format too long");
  var f = {
    justify: "+",
    signstyle: "-",
    filler: " ",
    alternate: false,
    base: 0,
    signedconv: false,
    width: 0,
    uppercase: false,
    sign: 1,
    prec: -1,
    conv: "f"
  };
  for (var i = 0; i < len; i++) {
    var c = fmt.charAt(i);
    switch (c) {
      case "-":
        f.justify = "-";
        break;
      case "+":
      case " ":
        f.signstyle = c;
        break;
      case "0":
        f.filler = "0";
        break;
      case "#":
        f.alternate = true;
        break;
      case "1":
      case "2":
      case "3":
      case "4":
      case "5":
      case "6":
      case "7":
      case "8":
      case "9":
        f.width = 0;
        while (((c = fmt.charCodeAt(i) - 48), c >= 0 && c <= 9)) {
          f.width = f.width * 10 + c;
          i++;
        }
        i--;
        break;
      case ".":
        f.prec = 0;
        i++;
        while (((c = fmt.charCodeAt(i) - 48), c >= 0 && c <= 9)) {
          f.prec = f.prec * 10 + c;
          i++;
        }
        i--;
      case "d":
      case "i":
        f.signedconv = true; /* fallthrough */
      case "u":
        f.base = 10;
        break;
      case "x":
        f.base = 16;
        break;
      case "X":
        f.base = 16;
        f.uppercase = true;
        break;
      case "o":
        f.base = 8;
        break;
      case "e":
      case "f":
      case "g":
        f.signedconv = true;
        f.conv = c;
        break;
      case "E":
      case "F":
      case "G":
        f.signedconv = true;
        f.uppercase = true;
        f.conv = c.toLowerCase();
        break;
    }
  }
  return f;
}

//Provides: caml_finish_formatting
//Requires: caml_new_string
function caml_finish_formatting(f, rawbuffer) {
  if (f.uppercase) rawbuffer = rawbuffer.toUpperCase();
  var len = rawbuffer.length;
  /* Adjust len to reflect additional chars (sign, etc) */
  if (f.signedconv && (f.sign < 0 || f.signstyle != "-")) len++;
  if (f.alternate) {
    if (f.base == 8) len += 1;
    if (f.base == 16) len += 2;
  }
  /* Do the formatting */
  var buffer = "";
  if (f.justify == "+" && f.filler == " ")
    for (var i = len; i < f.width; i++) buffer += " ";
  if (f.signedconv) {
    if (f.sign < 0) buffer += "-";
    else if (f.signstyle != "-") buffer += f.signstyle;
  }
  if (f.alternate && f.base == 8) buffer += "0";
  if (f.alternate && f.base == 16) buffer += "0x";
  if (f.justify == "+" && f.filler == "0")
    for (var i = len; i < f.width; i++) buffer += "0";
  buffer += rawbuffer;
  if (f.justify == "-") for (var i = len; i < f.width; i++) buffer += " ";
  console.log("FINISH formatting here...");
  return caml_new_string(buffer);
}

//Provides: caml_format_int const (const, const)
//Requires: caml_parse_format, caml_finish_formatting, caml_str_repeat
//Requires: caml_new_string, caml_jsbytes_of_string
function caml_format_int(fmt, i) {
  if (caml_jsbytes_of_string(fmt) == "%d") return caml_new_string("" + i);
  var f = caml_parse_format(fmt);
  if (i < 0) {
    if (f.signedconv) {
      f.sign = -1;
      i = -i;
    } else i >>>= 0;
  }
  var s = i.toString(f.base);
  if (f.prec >= 0) {
    f.filler = " ";
    var n = f.prec - s.length;
    if (n > 0) s = caml_str_repeat(n, "0") + s;
  }
  return caml_finish_formatting(f, s);
}

//Provides: caml_format_float const
//Requires: caml_parse_format, caml_finish_formatting
function caml_format_float(fmt, x) {
  var s,
    f = caml_parse_format(fmt);
  var prec = f.prec < 0 ? 6 : f.prec;
  if (x < 0 || (x == 0 && 1 / x == -Infinity)) {
    f.sign = -1;
    x = -x;
  }
  if (isNaN(x)) {
    s = "nan";
    f.filler = " ";
  } else if (!isFinite(x)) {
    s = "inf";
    f.filler = " ";
  } else
    switch (f.conv) {
      case "e":
        var s = x.toExponential(prec);
        // exponent should be at least two digits
        var i = s.length;
        if (s.charAt(i - 3) == "e")
          s = s.slice(0, i - 1) + "0" + s.slice(i - 1);
        break;
      case "f":
        s = x.toFixed(prec);
        break;
      case "g":
        prec = prec ? prec : 1;
        s = x.toExponential(prec - 1);
        var j = s.indexOf("e");
        var exp = +s.slice(j + 1);
        if (exp < -4 || x >= 1e21 || x.toFixed(0).length > prec) {
          // remove trailing zeroes
          var i = j - 1;
          while (s.charAt(i) == "0") i--;
          if (s.charAt(i) == ".") i--;
          s = s.slice(0, i + 1) + s.slice(j);
          i = s.length;
          if (s.charAt(i - 3) == "e")
            s = s.slice(0, i - 1) + "0" + s.slice(i - 1);
          break;
        } else {
          var p = prec;
          if (exp < 0) {
            p -= exp + 1;
            s = x.toFixed(p);
          } else while (((s = x.toFixed(p)), s.length > prec + 1)) p--;
          if (p) {
            // remove trailing zeroes
            var i = s.length - 1;
            while (s.charAt(i) == "0") i--;
            if (s.charAt(i) == ".") i--;
            s = s.slice(0, i + 1);
          }
        }
        break;
    }
  return caml_finish_formatting(f, s);
}

///////////// Hashtbl
//Provides: caml_hash_univ_param mutable
//Requires: MlBytes, caml_convert_string_to_bytes
//Requires: caml_int64_to_bytes, caml_int64_bits_of_float
function caml_hash_univ_param(count, limit, obj) {
  var hash_accu = 0;
  function hash_aux(obj) {
    limit--;
    if (count < 0 || limit < 0) return;
    if (obj instanceof Array && obj[0] === (obj[0] | 0)) {
      switch (obj[0]) {
        case 248:
          // Object
          count--;
          hash_accu = (hash_accu * 65599 + obj[2]) | 0;
          break;
        case 250:
          // Forward
          limit++;
          hash_aux(obj);
          break;
        case 255:
          // Int64
          count--;
          hash_accu = (hash_accu * 65599 + obj[1] + (obj[2] << 24)) | 0;
          break;
        default:
          count--;
          hash_accu = (hash_accu * 19 + obj[0]) | 0;
          for (var i = obj.length - 1; i > 0; i--) hash_aux(obj[i]);
      }
    } else if (obj instanceof MlBytes) {
      count--;
      switch (obj.t & 6) {
        default:
          /* PARTIAL */
          caml_convert_string_to_bytes(obj);
        case 0 /* BYTES */:
          for (var b = obj.c, l = obj.l, i = 0; i < l; i++)
            hash_accu = (hash_accu * 19 + b.charCodeAt(i)) | 0;
          break;
        case 2 /* ARRAY */:
          for (var a = obj.c, l = obj.l, i = 0; i < l; i++)
            hash_accu = (hash_accu * 19 + a[i]) | 0;
      }
    } else if (obj === (obj | 0)) {
      // Integer
      count--;
      hash_accu = (hash_accu * 65599 + obj) | 0;
    } else if (obj === +obj) {
      // Float
      count--;
      var p = caml_int64_to_bytes(caml_int64_bits_of_float(obj));
      for (var i = 7; i >= 0; i--) hash_accu = (hash_accu * 19 + p[i]) | 0;
    } else if (obj && obj.hash && typeof obj.hash === "function") {
      // Custom
      hash_accu = (hash_accu * 65599 + obj.hash()) | 0;
    }
  }
  hash_aux(obj);
  return hash_accu & 0x3fffffff;
}

//function ROTL32(x,n) { return ((x << n) | (x >>> (32-n))); }
//Provides: caml_hash_mix_int
//Requires: caml_mul
function caml_hash_mix_int(h, d) {
  d = caml_mul(d, 0xcc9e2d51 | 0);
  d = (d << 15) | (d >>> (32 - 15)); // ROTL32(d, 15);
  d = caml_mul(d, 0x1b873593);
  h ^= d;
  h = (h << 13) | (h >>> (32 - 13)); //ROTL32(h, 13);
  return (((h + (h << 2)) | 0) + (0xe6546b64 | 0)) | 0;
}

//Provides: caml_hash_mix_final
//Requires: caml_mul
function caml_hash_mix_final(h) {
  h ^= h >>> 16;
  h = caml_mul(h, 0x85ebca6b | 0);
  h ^= h >>> 13;
  h = caml_mul(h, 0xc2b2ae35 | 0);
  h ^= h >>> 16;
  return h;
}

//Provides: caml_hash_mix_float
//Requires: caml_hash_mix_int, caml_int64_bits_of_float
function caml_hash_mix_float(h, v0) {
  var v = caml_int64_bits_of_float(v0);
  var lo = v[1] | (v[2] << 24);
  var hi = (v[2] >>> 8) | (v[3] << 16);
  h = caml_hash_mix_int(h, lo);
  h = caml_hash_mix_int(h, hi);
  return h;
}
//Provides: caml_hash_mix_int64
//Requires: caml_hash_mix_int
function caml_hash_mix_int64(h, v) {
  var lo = v[1] | (v[2] << 24);
  var hi = (v[2] >>> 8) | (v[3] << 16);
  h = caml_hash_mix_int(h, hi ^ lo);
  return h;
}

//Provides: caml_hash_mix_string_str
//Requires: caml_hash_mix_int
function caml_hash_mix_string_str(h, s) {
  var len = s.length,
    i,
    w;
  for (i = 0; i + 4 <= len; i += 4) {
    w =
      s.charCodeAt(i) |
      (s.charCodeAt(i + 1) << 8) |
      (s.charCodeAt(i + 2) << 16) |
      (s.charCodeAt(i + 3) << 24);
    h = caml_hash_mix_int(h, w);
  }
  w = 0;
  switch (len & 3) {
    case 3:
      w = s.charCodeAt(i + 2) << 16;
    case 2:
      w |= s.charCodeAt(i + 1) << 8;
    case 1:
      w |= s.charCodeAt(i);
      h = caml_hash_mix_int(h, w);
    default:
  }
  h ^= len;
  return h;
}

//Provides: caml_hash_mix_string_arr
//Requires: caml_hash_mix_int
function caml_hash_mix_string_arr(h, s) {
  var len = s.length,
    i,
    w;
  for (i = 0; i + 4 <= len; i += 4) {
    w = s[i] | (s[i + 1] << 8) | (s[i + 2] << 16) | (s[i + 3] << 24);
    h = caml_hash_mix_int(h, w);
  }
  w = 0;
  switch (len & 3) {
    case 3:
      w = s[i + 2] << 16;
    case 2:
      w |= s[i + 1] << 8;
    case 1:
      w |= s[i];
      h = caml_hash_mix_int(h, w);
    default:
  }
  h ^= len;
  return h;
}

//Provides: caml_hash_mix_string
//Requires: caml_convert_string_to_bytes
//Requires: caml_hash_mix_string_str
//Requires: caml_hash_mix_string_arr
function caml_hash_mix_string(h, v) {
  switch (v.t & 6) {
    default:
      caml_convert_string_to_bytes(v);
    case 0 /* BYTES */:
      h = caml_hash_mix_string_str(h, v.c);
      break;
    case 2 /* ARRAY */:
      h = caml_hash_mix_string_arr(h, v.c);
  }
  return h;
}

//Provides: caml_hash mutable
//Requires: MlBytes
//Requires: caml_int64_bits_of_float, caml_hash_mix_int, caml_hash_mix_final
//Requires: caml_hash_mix_int64, caml_hash_mix_float, caml_hash_mix_string
var HASH_QUEUE_SIZE = 256;
function caml_hash(count, limit, seed, obj) {
  var queue, rd, wr, sz, num, h, v, i, len;
  sz = limit;
  if (sz < 0 || sz > HASH_QUEUE_SIZE) sz = HASH_QUEUE_SIZE;
  num = count;
  h = seed;
  queue = [obj];
  rd = 0;
  wr = 1;
  while (rd < wr && num > 0) {
    v = queue[rd++];
    if (v instanceof Array && v[0] === (v[0] | 0)) {
      switch (v[0]) {
        case 248:
          // Object
          h = caml_hash_mix_int(h, v[2]);
          num--;
          break;
        case 250:
          // Forward
          queue[--rd] = v[1];
          break;
        case 255:
          // Int64
          h = caml_hash_mix_int64(h, v);
          num--;
          break;
        default:
          var tag = ((v.length - 1) << 10) | v[0];
          h = caml_hash_mix_int(h, tag);
          for (i = 1, len = v.length; i < len; i++) {
            if (wr >= sz) break;
            queue[wr++] = v[i];
          }
          break;
      }
    } else if (v instanceof MlBytes) {
      h = caml_hash_mix_string(h, v);
      num--;
    } else if (v === (v | 0)) {
      // Integer
      h = caml_hash_mix_int(h, v + v + 1);
      num--;
    } else if (v === +v) {
      // Float
      h = caml_hash_mix_float(h, v);
      num--;
    } else if (v && v.hash && typeof v.hash === "function") {
      // Custom
      h = caml_hash_mix_int(h, v.hash());
    }
  }
  h = caml_hash_mix_final(h);
  return h & 0x3fffffff;
}

///////////// Sys
//Provides: caml_sys_time mutable
var caml_initial_time = new Date() * 0.001;
function caml_sys_time() {
  return new Date() * 0.001 - caml_initial_time;
}
//Provides: caml_sys_get_config const
//Requires: caml_new_string
function caml_sys_get_config() {
  return [0, caml_new_string("Unix"), 32, 0];
}

//Provides: caml_sys_const_backend_type const
//Requires: caml_new_string
function caml_sys_const_backend_type() {
  return [0, caml_new_string("js_of_ocaml")];
}

//Provides: caml_sys_random_seed mutable
//Version: < 4.00
//The function needs to return an array since OCaml 4.0...
function caml_sys_random_seed() {
  var x = new Date() ^ (0xffffffff * Math.random());
  return x;
}

//Provides: caml_sys_random_seed mutable
//Version: >= 4.00
//The function needs to return an array since OCaml 4.0...
function caml_sys_random_seed() {
  var x = new Date() ^ (0xffffffff * Math.random());
  return [0, x];
}

//Provides: caml_sys_const_big_endian const
function caml_sys_const_big_endian() {
  return 0;
}
//Provides: caml_sys_const_word_size const
function caml_sys_const_word_size() {
  return 32;
}
//Provides: caml_sys_const_int_size const
function caml_sys_const_int_size() {
  return 32;
}

//Provides: caml_sys_const_max_wosize const
// max_int / 4 so that the following does not overflow
//let max_string_length = word_size / 8 * max_array_length - 1;;
function caml_sys_const_max_wosize() {
  return (0x7fffffff / 4) | 0;
}

//Provides: caml_sys_const_ostype_cygwin const
function caml_sys_const_ostype_cygwin() {
  return 0;
}
//Provides: caml_sys_const_ostype_unix const
function caml_sys_const_ostype_unix() {
  return 1;
}
//Provides: caml_sys_const_ostype_win32 const
function caml_sys_const_ostype_win32() {
  return 0;
}

//Provides: caml_sys_system_command
function caml_sys_system_command(cmd) {
  var cmd = cmd.toString();
  joo_global_object.console.log(cmd);
  if (
    typeof require != "undefined" &&
    require("child_process") &&
    require("child_process").execSync
  ) {
    try {
      require("child_process").execSync(cmd);
      return 0;
    } catch (e) {
      return 1;
    }
  } else return 127;
}

///////////// Array
//Provides: caml_array_sub mutable
function caml_array_sub(a, i, len) {
  var a2 = new Array(len + 1);
  a2[0] = 0;
  for (var i2 = 1, i1 = i + 1; i2 <= len; i2++, i1++) {
    a2[i2] = a[i1];
  }
  return a2;
}

//Provides: caml_array_append mutable
function caml_array_append(a1, a2) {
  var l1 = a1.length,
    l2 = a2.length;
  var l = l1 + l2 - 1;
  var a = new Array(l);
  a[0] = 0;
  var i = 1,
    j = 1;
  for (; i < l1; i++) a[i] = a1[i];
  for (; i < l; i++, j++) a[i] = a2[j];
  return a;
}

//Provides: caml_array_concat mutable
function caml_array_concat(l) {
  var a = [0];
  while (l !== 0) {
    var b = l[1];
    for (var i = 1; i < b.length; i++) a.push(b[i]);
    l = l[2];
  }
  return a;
}

//Provides: caml_array_blit
function caml_array_blit(a1, i1, a2, i2, len) {
  if (i2 <= i1) {
    for (var j = 1; j <= len; j++) a2[i2 + j] = a1[i1 + j];
  } else {
    for (var j = len; j >= 1; j--) a2[i2 + j] = a1[i1 + j];
  }
  return 0;
}

///////////// CamlinternalOO
//Provides: caml_get_public_method const
var caml_method_cache = [];
function caml_get_public_method(obj, tag, cacheid) {
  var meths = obj[1];
  var ofs = caml_method_cache[cacheid];
  if (ofs === null) {
    // Make sure the array is not sparse
    for (var i = caml_method_cache.length; i < cacheid; i++)
      caml_method_cache[i] = 0;
  } else if (meths[ofs] === tag) {
    return meths[ofs - 1];
  }
  var li = 3,
    hi = meths[1] * 2 + 1,
    mi;
  while (li < hi) {
    mi = ((li + hi) >> 1) | 1;
    if (tag < meths[mi + 1]) hi = mi - 2;
    else li = mi;
  }
  caml_method_cache[cacheid] = li + 1;
  /* return 0 if tag is not there */
  return tag == meths[li + 1] ? meths[li] : 0;
}

//Provides: caml_final_register const
function caml_final_register() {
  return 0;
}
//Provides: caml_final_register_called_without_value const
function caml_final_register_called_without_value() {
  return 0;
}
//Provides: caml_final_release const
function caml_final_release() {
  return 0;
}
//Provides: caml_backtrace_status const
function caml_backtrace_status() {
  return 0;
}
//Provides: caml_get_exception_backtrace const
function caml_get_exception_backtrace() {
  return 0;
}
//Provides: caml_get_exception_raw_backtrace const
function caml_get_exception_raw_backtrace() {
  return [0];
}
//Provides: caml_record_backtrace
function caml_record_backtrace() {
  return 0;
}
//Provides: caml_convert_raw_backtrace const
function caml_convert_raw_backtrace() {
  return [0];
}
//Provides: caml_raw_backtrace_length
function caml_raw_backtrace_length() {
  return 0;
}
//Provides: caml_raw_backtrace_next_slot
function caml_raw_backtrace_next_slot() {
  return 0;
}
//Provides: caml_raw_backtrace_slot
//Requires: caml_invalid_argument
function caml_raw_backtrace_slot() {
  caml_invalid_argument("Printexc.get_raw_backtrace_slot: index out of bounds");
}
//Provides: caml_restore_raw_backtrace
function caml_restore_raw_backtrace(exn, bt) {
  return 0;
}
//Provides: caml_get_current_callstack const
function caml_get_current_callstack() {
  return [0];
}
//Provides: caml_sys_getenv (const)
//Requires: caml_raise_not_found
//Requires: caml_js_to_string
function caml_sys_getenv(name) {
  var g = joo_global_object;
  var n = name.toString();
  //nodejs env
  if (g.process && g.process.env && g.process.env[n] != undefined)
    return caml_js_to_string(g.process.env[n]);
  caml_raise_not_found();
}
//Provides: caml_sys_exit
//Requires: caml_invalid_argument
function caml_sys_exit(code) {
  var g = joo_global_object;
  if (g.quit) g.quit(code);
  //nodejs
  if (g.process && g.process.exit) g.process.exit(code);
  caml_invalid_argument("Function 'exit' not implemented");
}

//Provides: caml_sys_get_argv const
//Requires: caml_js_to_string
//Requires: raw_array_sub
function caml_sys_get_argv() {
  var g = joo_global_object;
  var main = "a.out";
  var args = [];

  if (g.process && g.process.argv && g.process.argv.length > 1) {
    var argv = g.process.argv;
    //nodejs
    main = argv[1];
    args = raw_array_sub(argv, 2, argv.length - 2);
  }

  var p = caml_js_to_string(main);
  var args2 = [0, p];
  for (var i = 0; i < args.length; i++) args2.push(caml_js_to_string(args[i]));
  return [0, p, args2];
}

//Provides: unix_inet_addr_of_string
function unix_inet_addr_of_string() {
  return 0;
}

//Provides: caml_oo_last_id
var caml_oo_last_id = 0;

//Provides: caml_set_oo_id
//Requires: caml_oo_last_id
function caml_set_oo_id(b) {
  b[2] = caml_oo_last_id++;
  return b;
}

//Provides: caml_fresh_oo_id
//Requires: caml_oo_last_id
function caml_fresh_oo_id() {
  return caml_oo_last_id++;
}

//Provides: caml_install_signal_handler const
function caml_install_signal_handler() {
  return 0;
}

//Provides: caml_convert_raw_backtrace_slot
//Requires: caml_failwith
function caml_convert_raw_backtrace_slot() {
  caml_failwith("caml_convert_raw_backtrace_slot");
}

//Provides: caml_bswap16
function caml_bswap16(x) {
  return ((x & 0x00ff) << 8) | ((x & 0xff00) >> 8);
}
//Provides: caml_int32_bswap
function caml_int32_bswap(x) {
  return (
    ((x & 0x000000ff) << 24) |
    ((x & 0x0000ff00) << 8) |
    ((x & 0x00ff0000) >>> 8) |
    ((x & 0xff000000) >>> 24)
  );
}
//Provides: caml_int64_bswap
function caml_int64_bswap(x) {
  return [
    255,
    ((x[3] & 0x0000ff00) >> 8) |
      ((x[3] & 0x000000ff) << 8) |
      (x[2] & 0x00ff0000),
    ((x[2] & 0x0000ff00) >> 8) |
      ((x[2] & 0x000000ff) << 8) |
      (x[1] & 0x00ff0000),
    ((x[1] & 0x0000ff00) >> 8) | ((x[1] & 0x000000ff) << 8)
  ];
}

//Provides: caml_list_of_js_array const (const)
function caml_list_of_js_array(a) {
  var l = 0;
  for (var i = a.length - 1; i >= 0; i--) {
    var e = a[i];
    l = [0, e, l];
  }
  return l;
}

//Provides: caml_runtime_warnings
var caml_runtime_warnings = 0;

//Provides: caml_ml_enable_runtime_warnings
//Requires: caml_runtime_warnings
function caml_ml_enable_runtime_warnings(bool) {
  caml_runtime_warnings = bool;
  return 0;
}

//Provides: caml_ml_runtime_warnings_enabled
//Requires: caml_runtime_warnings
function caml_ml_runtime_warnings_enabled(_unit) {
  return caml_runtime_warnings;
}

//Provides: caml_runtime_variant
//Requires: caml_new_string
function caml_runtime_variant(_unit) {
  return caml_new_string("");
}
//Provides: caml_runtime_parameters
//Requires: caml_new_string
function caml_runtime_parameters(_unit) {
  return caml_new_string("");
}

//Provides: caml_sys_isatty
function caml_sys_isatty(_chan) {
  return 0;
}

//Provides: caml_spacetime_enabled const (const)
function caml_spacetime_enabled(_unit) {
  return 0;
}

//Provides: caml_register_channel_for_spacetime const (const)
function caml_register_channel_for_spacetime(_channel) {
  return 0;
}

//Provides: caml_spacetime_only_works_for_native_code
//Requires: caml_failwith
function caml_spacetime_only_works_for_native_code() {
  caml_failwith("Spacetime profiling only works for native code");
}

//Provides: caml_is_js
function caml_is_js() {
  return 1;
}

// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2014 Jérôme Vouillon, Hugo Heuzard
// Laboratoire PPS - CNRS Université Paris Diderot
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

///////////// Io

//Provides: caml_sys_close
//Requires: caml_global_data
function caml_sys_close(fd) {
  delete caml_global_data.fds[fd];
  return 0;
}

//Provides: caml_std_output
//Requires: caml_new_string, caml_ml_string_length, caml_ml_channels
function caml_std_output(chanid, s) {
  var chan = caml_ml_channels[chanid];
  var str = caml_new_string(s);
  var slen = caml_ml_string_length(str);
  chan.file.write(chan.offset, str, 0, slen);
  chan.offset += slen;
  return 0;
}

//Provides: caml_sys_open
//Requires: caml_raise_sys_error, caml_global_data
//Requires: caml_create_bytes,MlFakeFile
//Requires: js_print_stderr, js_print_stdout
//Requires: caml_std_output
//Requires: resolve_fs_device
function caml_sys_open_internal(idx, output, file, flags) {
  if (caml_global_data.fds === undefined) caml_global_data.fds = new Array();
  flags = flags ? flags : {};
  var info = {};
  info.file = file;
  info.offset = flags.append ? file.length() : 0;
  info.flags = flags;
  info.output = output;
  caml_global_data.fds[idx] = info;
  if (!caml_global_data.fd_last_idx || idx > caml_global_data.fd_last_idx)
    caml_global_data.fd_last_idx = idx;
  return idx;
}
function caml_sys_open(name, flags, _perms) {
  var f = {};
  while (flags) {
    switch (flags[1]) {
      case 0:
        f.rdonly = 1;
        break;
      case 1:
        f.wronly = 1;
        break;
      case 2:
        f.append = 1;
        break;
      case 3:
        f.create = 1;
        break;
      case 4:
        f.truncate = 1;
        break;
      case 5:
        f.excl = 1;
        break;
      case 6:
        f.binary = 1;
        break;
      case 7:
        f.text = 1;
        break;
      case 8:
        f.nonblock = 1;
        break;
    }
    flags = flags[2];
  }
  if (f.rdonly && f.wronly)
    caml_raise_sys_error(
      name.toString() +
        " : flags Open_rdonly and Open_wronly are not compatible"
    );
  if (f.text && f.binary)
    caml_raise_sys_error(
      name.toString() + " : flags Open_text and Open_binary are not compatible"
    );
  var root = resolve_fs_device(name);
  var file = root.device.open(root.rest, f);
  var idx = caml_global_data.fd_last_idx ? caml_global_data.fd_last_idx : 0;
  return caml_sys_open_internal(idx + 1, caml_std_output, file, f);
}
caml_sys_open_internal(
  0,
  caml_std_output,
  new MlFakeFile(caml_create_bytes(0))
); //stdin
caml_sys_open_internal(
  1,
  js_print_stdout,
  new MlFakeFile(caml_create_bytes(0))
); //stdout
caml_sys_open_internal(
  2,
  js_print_stderr,
  new MlFakeFile(caml_create_bytes(0))
); //stderr

// ocaml Channels

//Provides: caml_ml_set_channel_name
function caml_ml_set_channel_name() {
  return 0;
}

//Provides: caml_ml_channels
var caml_ml_channels = new Array();

//Provides: caml_ml_out_channels_list
//Requires: caml_ml_channels
function caml_ml_out_channels_list() {
  var l = 0;
  for (var c = 0; c < caml_ml_channels.length; c++) {
    if (
      caml_ml_channels[c] &&
      caml_ml_channels[c].opened &&
      caml_ml_channels[c].out
    )
      l = [0, caml_ml_channels[c].fd, l];
  }
  return l;
}

// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2014 Jérôme Vouillon, Hugo Heuzard
// Laboratoire PPS - CNRS Université Paris Diderot
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

//Provides: MlFakeDevice
//Requires: MlFakeFile, caml_create_bytes
//Requires: caml_raise_sys_error, caml_raise_no_such_file, caml_new_string, caml_string_of_array
//Requires: MlBytes
function MlFakeDevice(root, f) {
  this.content = {};
  this.root = root;
  this.lookupFun = f;
}
MlFakeDevice.prototype.nm = function(name) {
  return this.root + name;
};
MlFakeDevice.prototype.lookup = function(name) {
  if (!this.content[name] && this.lookupFun) {
    var res = this.lookupFun(caml_new_string(this.root), caml_new_string(name));
    if (res != 0) this.content[name] = new MlFakeFile(res[1]);
  }
};
MlFakeDevice.prototype.exists = function(name) {
  // The root of the device exists
  if (name == "") return 1;
  // Check if a directory exists
  var name_slash = name + "/";
  var r = new RegExp("^" + name_slash);
  for (var n in this.content) {
    if (n.match(r)) return 1;
  }
  // Check if a file exists
  this.lookup(name);
  return this.content[name] ? 1 : 0;
};
MlFakeDevice.prototype.readdir = function(name) {
  var name_slash = name == "" ? "" : name + "/";
  var r = new RegExp("^" + name_slash + "([^/]*)");
  var seen = {};
  var a = [];
  for (var n in this.content) {
    var m = n.match(r);
    if (m && !seen[m[1]]) {
      seen[m[1]] = true;
      a.push(m[1]);
    }
  }
  return a;
};
MlFakeDevice.prototype.is_dir = function(name) {
  var name_slash = name == "" ? "" : name + "/";
  var r = new RegExp("^" + name_slash + "([^/]*)");
  var a = [];
  for (var n in this.content) {
    var m = n.match(r);
    if (m) return 1;
  }
  return 0;
};
MlFakeDevice.prototype.unlink = function(name) {
  var ok = this.content[name] ? true : false;
  delete this.content[name];
  return ok;
};
MlFakeDevice.prototype.open = function(name, f) {
  if (f.rdonly && f.wronly)
    caml_raise_sys_error(
      this.nm(name) + " : flags Open_rdonly and Open_wronly are not compatible"
    );
  if (f.text && f.binary)
    caml_raise_sys_error(
      this.nm(name) + " : flags Open_text and Open_binary are not compatible"
    );
  this.lookup(name);
  if (this.content[name]) {
    if (this.is_dir(name))
      caml_raise_sys_error(this.nm(name) + " : is a directory");
    if (f.create && f.excl)
      caml_raise_sys_error(this.nm(name) + " : file already exists");
    var file = this.content[name];
    if (f.truncate) file.truncate();
    return file;
  } else if (f.create) {
    this.content[name] = new MlFakeFile(caml_create_bytes(0));
    return this.content[name];
  } else {
    caml_raise_no_such_file(this.nm(name));
  }
};

MlFakeDevice.prototype.register = function(name, content) {
  if (this.content[name])
    caml_raise_sys_error(this.nm(name) + " : file already exists");
  if (content instanceof MlBytes) this.content[name] = new MlFakeFile(content);
  else if (content instanceof Array)
    this.content[name] = new MlFakeFile(caml_string_of_array(content));
  else if (content.toString) {
    var mlstring = caml_new_string(content.toString());
    this.content[name] = new MlFakeFile(mlstring);
  }
};

MlFakeDevice.prototype.constructor = MlFakeDevice;

//Provides: MlFakeFile
//Requires: MlFile
//Requires: caml_create_bytes, caml_ml_bytes_length,caml_blit_bytes
//Requires: caml_bytes_get
function MlFakeFile(content) {
  this.data = content;
}
MlFakeFile.prototype = new MlFile();
MlFakeFile.prototype.truncate = function(len) {
  var old = this.data;
  this.data = caml_create_bytes(len | 0);
  caml_blit_bytes(old, 0, this.data, 0, len);
};
MlFakeFile.prototype.length = function() {
  return caml_ml_bytes_length(this.data);
};
MlFakeFile.prototype.write = function(offset, buf, pos, len) {
  var clen = this.length();
  if (offset + len >= clen) {
    var new_str = caml_create_bytes(offset + len);
    var old_data = this.data;
    this.data = new_str;
    caml_blit_bytes(old_data, 0, this.data, 0, clen);
  }
  caml_blit_bytes(buf, pos, this.data, offset, len);
  return 0;
};
MlFakeFile.prototype.read = function(offset, buf, pos, len) {
  var clen = this.length();
  caml_blit_bytes(this.data, offset, buf, pos, len);
  return 0;
};
MlFakeFile.prototype.read_one = function(offset) {
  return caml_bytes_get(this.data, offset);
};
MlFakeFile.prototype.close = function() {};
MlFakeFile.prototype.constructor = MlFakeFile;

//Provides: caml_ml_open_descriptor_out
//Requires: caml_ml_channels, caml_global_data
//Requires: caml_raise_sys_error
function caml_ml_open_descriptor_out(fd) {
  var data = caml_global_data.fds[fd];
  if (data.flags.rdonly) caml_raise_sys_error("fd " + fd + " is readonly");
  var channel = {
    file: data.file,
    offset: data.offset,
    fd: fd,
    opened: true,
    out: true,
    buffer: ""
  };
  caml_ml_channels[channel.fd] = channel;
  return channel.fd;
}

//Provides: caml_ml_open_descriptor_in
//Requires: caml_global_data,caml_sys_open,caml_raise_sys_error, caml_ml_channels
function caml_ml_open_descriptor_in(fd) {
  var data = caml_global_data.fds[fd];
  if (data.flags.wronly) caml_raise_sys_error("fd " + fd + " is writeonly");

  var channel = {
    file: data.file,
    offset: data.offset,
    fd: fd,
    opened: true,
    out: false,
    refill: null
  };

  console.info("create channel:", channel.fd);

  caml_ml_channels[channel.fd] = channel;
  return channel.fd;
}

//Provides: caml_ml_set_binary_mode
//Requires: caml_global_data, caml_ml_channels
function caml_ml_set_binary_mode(chanid, mode) {
  var chan = caml_ml_channels[chanid];
  var data = caml_global_data.fds[chan.fd];
  data.flags.text = !mode;
  data.flags.binary = mode;
  return 0;
}

//Input from in_channel

//Provides: caml_ml_close_channel
//Requires: caml_ml_flush, caml_ml_channels
//Requires: caml_sys_close
function caml_ml_close_channel(chanid) {
  var chan = caml_ml_channels[chanid];
  caml_ml_flush(chanid);
  chan.opened = false;
  chan.file.close();
  caml_sys_close(chan.fd);
  return 0;
}

//Provides: caml_ml_channel_size
//Requires: caml_ml_channels
function caml_ml_channel_size(chanid) {
  var chan = caml_ml_channels[chanid];
  return chan.file.length();
}

//Provides: caml_ml_channel_size_64
//Requires: caml_int64_of_float,caml_ml_channels
function caml_ml_channel_size_64(chanid) {
  var chan = caml_ml_channels[chanid];
  return caml_int64_of_float(chan.file.length());
}

//Provides: caml_ml_set_channel_output
//Requires: caml_ml_channels, caml_global_data
function caml_ml_set_channel_output(chanid, f) {
  var chan = caml_ml_channels[chanid];
  caml_global_data.fds[chan.fd].output = f;
  return 0;
}

//Provides: caml_ml_set_channel_refill
//Requires: caml_ml_channels, caml_global_data
function caml_ml_set_channel_refill(chanid, f) {
  caml_ml_channels[chanid].refill = f;
  return 0;
}

//Provides: caml_ml_refill_input
//Requires: caml_ml_bytes_length
function caml_ml_refill_input(chan) {
  var str = chan.refill();
  var str_len = caml_ml_bytes_length(str);
  if (str_len == 0) chan.refill = null;
  chan.file.write(chan.file.length(), str, 0, str_len);
  return str_len;
}

//Provides: caml_ml_may_refill_input
//Requires: caml_ml_refill_input, caml_ml_channels
function caml_ml_may_refill_input(chanid) {
  var chan = caml_ml_channels[chanid];
  if (chan.refill == null) return;
  if (chan.file.length() != chan.offset) return;
  caml_ml_refill_input(chan);
}

//Provides: caml_ml_input
//Requires: caml_ml_refill_input, caml_ml_channels
function caml_ml_input(chanid, s, i, l) {
  var chan = caml_ml_channels[chanid];
  var l2 = chan.file.length() - chan.offset;
  if (l2 == 0 && chan.refill != null) l2 = caml_ml_refill_input(chan);
  if (l2 < l) l = l2;
  chan.file.read(chan.offset, s, i, l);
  chan.offset += l;
  return l;
}

//Provides: caml_input_value
//Requires: caml_marshal_data_size, caml_input_value_from_string, caml_create_bytes, caml_ml_channels
function caml_input_value(chanid) {
  var chan = caml_ml_channels[chanid];

  var buf = caml_create_bytes(8);
  chan.file.read(chan.offset, buf, 0, 8);

  // Header is 20 bytes
  var len = caml_marshal_data_size(buf, 0) + 20;

  var buf = caml_create_bytes(len);
  chan.file.read(chan.offset, buf, 0, len);

  var offset = [0];
  var res = caml_input_value_from_string(buf, offset);
  chan.offset = chan.offset + offset[0];
  return res;
}

//Provides: caml_ml_input_char
//Requires: caml_raise_end_of_file, caml_array_bound_error
//Requires: caml_ml_may_refill_input, caml_ml_channels
function caml_ml_input_char(chanid) {
  var chan = caml_ml_channels[chanid];
  caml_ml_may_refill_input(chanid);
  if (chan.offset >= chan.file.length()) caml_raise_end_of_file();
  var res = chan.file.read_one(chan.offset);
  chan.offset++;
  return res;
}

//Provides: caml_ml_input_int
//Requires: caml_raise_end_of_file
//Requires: caml_ml_refill_input, caml_ml_channels
function caml_ml_input_int(chanid) {
  var chan = caml_ml_channels[chanid];
  var file = chan.file;
  while (chan.offset + 3 >= file.length()) {
    var l = caml_ml_refill_input(chan);
    if (l == 0) caml_raise_end_of_file();
  }
  var o = chan.offset;
  var r =
    (file.read_one(o) << 24) |
    (file.read_one(o + 1) << 16) |
    (file.read_one(o + 2) << 8) |
    file.read_one(o + 3);
  chan.offset += 4;
  return r;
}

//Provides: caml_ml_seek_in
//Requires: caml_raise_sys_error, caml_ml_channels
function caml_ml_seek_in(chanid, pos) {
  var chan = caml_ml_channels[chanid];
  if (chan.refill != null) caml_raise_sys_error("Illegal seek");
  chan.offset = pos;
  return 0;
}

//Provides: caml_ml_seek_in_64
//Requires: caml_int64_to_float, caml_raise_sys_error, caml_ml_channels
function caml_ml_seek_in_64(chanid, pos) {
  var chan = caml_ml_channels[chanid];
  if (chan.refill != null) caml_raise_sys_error("Illegal seek");
  chan.offset = caml_int64_to_float(pos);
  return 0;
}

//Provides: caml_ml_pos_in
//Requires: caml_ml_channels
function caml_ml_pos_in(chanid) {
  return caml_ml_channels[chanid].offset;
}

//Provides: caml_ml_pos_in_64
//Requires: caml_int64_of_float, caml_ml_channels
function caml_ml_pos_in_64(chanid) {
  return caml_int64_of_float(caml_ml_channels[chanid].offset);
}

//Provides: caml_ml_input_scan_line
//Requires: caml_array_bound_error
//Requires: caml_ml_may_refill_input, caml_ml_channels
function caml_ml_input_scan_line(chanid) {
  var chan = caml_ml_channels[chanid];
  caml_ml_may_refill_input(chanid);
  var p = chan.offset;
  var len = chan.file.length();
  if (p >= len) {
    return 0;
  }
  while (true) {
    if (p >= len) return -(p - chan.offset);
    if (chan.file.read_one(p) == 10) return p - chan.offset + 1;
    p++;
  }
}

//Provides: caml_ml_flush
//Requires: caml_raise_sys_error, caml_global_data, caml_ml_channels
function caml_ml_flush(chanid) {
  var chan = caml_ml_channels[chanid];
  if (!chan.opened) caml_raise_sys_error("Cannot flush a closed channel");
  if (!chan.buffer || chan.buffer == "") return 0;
  if (
    chan.fd &&
    caml_global_data.fds[chan.fd] &&
    caml_global_data.fds[chan.fd].output
  ) {
    var output = caml_global_data.fds[chan.fd].output;
    switch (output.length) {
      case 2:
        output(chanid, chan.buffer);
        break;
      default:
        output(chan.buffer);
    }
  }
  chan.buffer = "";
  return 0;
}

//output to out_channel

//Provides: caml_ml_output_bytes
//Requires: caml_ml_flush,caml_ml_bytes_length
//Requires: caml_create_bytes, caml_blit_bytes, caml_raise_sys_error, caml_ml_channels, caml_jsbytes_of_string
function caml_ml_output_bytes(chanid, buffer, offset, len) {
  var chan = caml_ml_channels[chanid];
  if (!chan.opened) caml_raise_sys_error("Cannot output to a closed channel");
  var string;
  if (offset == 0 && caml_ml_bytes_length(buffer) == len) string = buffer;
  else {
    string = caml_create_bytes(len);
    caml_blit_bytes(buffer, offset, string, 0, len);
  }
  var jsstring = caml_jsbytes_of_string(string);
  var id = jsstring.lastIndexOf("\n");
  if (id < 0) chan.buffer += jsstring;
  else {
    chan.buffer += jsstring.substr(0, id + 1);
    caml_ml_flush(chanid);
    chan.buffer += jsstring.substr(id + 1);
  }
  return 0;
}

//Provides: caml_ml_output
//Requires: caml_ml_output_bytes
function caml_ml_output(chanid, buffer, offset, len) {
  console.log("caml_ml_output");
  return caml_ml_output_bytes(chanid, buffer, offset, len);
}

//Provides: caml_ml_output_char
//Requires: caml_ml_output
//Requires: caml_new_string
function caml_ml_output_char(chanid, c) {
  var s = caml_new_string(String.fromCharCode(c));
  caml_ml_output(chanid, s, 0, 1);
  return 0;
}

//Provides: caml_output_value
//Requires: caml_output_value_to_string, caml_ml_output,caml_ml_string_length
function caml_output_value(chanid, v, _flags) {
  var s = caml_output_value_to_string(v);
  caml_ml_output(chanid, s, 0, caml_ml_string_length(s));
  return 0;
}

//Provides: caml_ml_seek_out
//Requires: caml_ml_channels
function caml_ml_seek_out(chanid, pos) {
  caml_ml_channels[chanid].offset = pos;
  return 0;
}

//Provides: caml_ml_seek_out_64
//Requires: caml_int64_to_float, caml_ml_channels
function caml_ml_seek_out_64(chanid, pos) {
  caml_ml_channels[chanid].offset = caml_int64_to_float(pos);
  return 0;
}

//Provides: caml_ml_pos_out
//Requires: caml_ml_channels
function caml_ml_pos_out(chanid) {
  return caml_ml_channels[chanid].offset;
}

//Provides: caml_ml_pos_out_64
//Requires: caml_int64_of_float, caml_ml_channels
function caml_ml_pos_out_64(chanid) {
  return caml_int64_of_float(caml_ml_chnnels[chanid].offset);
}

//Provides: caml_ml_output_int
//Requires: caml_ml_output
//Requires: caml_string_of_array
function caml_ml_output_int(chanid, i) {
  var arr = [(i >> 24) & 0xff, (i >> 16) & 0xff, (i >> 8) & 0xff, i & 0xff];
  var s = caml_string_of_array(arr);
  caml_ml_output(chanid, s, 0, 4);
  return 0;
}

function extract(instance, value) {
  if (value & 1) {
    // number
    return value >> 1;
  } else {
    // pointer
    const blockheaderPosition = value - 4;
    var blockheaderBytes = new Uint32Array(
      instance.exports.memory.buffer,
      blockheaderPosition,
      4
    );
    const tag = blockheaderBytes[0] & 255;
    switch (tag) {
      case 252:
        const l = (blockheaderBytes[0] >> 10) * 4;
        const stringBytes = new Uint8Array(
          instance.exports.memory.buffer,
          value
        );
        let padding = stringBytes[l - 1] + 1;
        console.log(padding);
        const str = new Uint8Array(
          instance.exports.memory.buffer,
          value,
          l - padding
        );
        var string = td.decode(str);
        return {
          t: 9,
          l: string.length,
          c: string
        };
      case 247:
        const length = blockheaderBytes[0] >> 9;
        console.log("l:", length, value);
      default:
        console.error("not handled yet:", tag);
    }
  }
}

function blockheader(tag, wosize, color) {
  const result = tag + (color << 8) + (wosize << 10);
  return result;
}

var te = new TextEncoder("utf-8");
function convert(o) {
  if (typeof o === "string") {
    const rest = o.length % 4;
    const padding = rest === 3 ? 0 : rest === 2 ? 1 : rest === 1 ? 2 : 3;
    const length = o.length + (4 - rest);

    const pointer = env.caml_alloc(4 + length);
    var i8 = new Uint8Array(env.instance.exports.memory.buffer);
    var i32 = new Uint32Array(env.instance.exports.memory.buffer);
    var encodedString = te.encode(o);
    var result = blockheader(252, length / 4, 3);

    i32.set([result], pointer / 4);

    i8.set(encodedString, pointer + 4);
    if (padding > 0) {
      i8.set([0], pointer + length + 4 - 1);
    }
    if (padding > 1) {
      i8.set([0], pointer + length + 4 - 2);
    }
    if (padding > 2) {
      i8.set([0], pointer + length + 4 - 3);
    }
    i8.set([padding], pointer + length + 3);

    return pointer + 4;
  } else {
    console.error("cannot handle this yet sorry...");
  }
}

var env = {
  alloc: 0,
  instance: null,
  caml_curry2: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry3: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry4: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry5: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry6: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry7: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry8: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry9: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry10: function() {
    throw new Error("Not implemented yet");
  },
  caml_curry11: function() {
    throw new Error("Not implemented yet");
  },
  caml_apply4: function() {
    throw new Error("Not implemented yet");
  },
  caml_apply5: function() {
    throw new Error("Not implemented yet");
  },
  js_int_new: function(val) {
    console.log(`Allocating an int (${val})...`);
    return val; // will be received as anyref instead of i32
  },
  caml_alloc: function(amount) {
    const result = env.alloc;
    env.alloc += amount;
    return result;
  },
  caml_ml_set_binary_mode: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_close_channel: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_channel_size: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_pos_in: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_seek_in: function() {
    throw new Error("Not implemented yet");
  },
  caml_input_value: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_input_int: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_input_char: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_pos_out: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_seek_out: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_output_int: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_output_char: function(chanid, c) {
    return ocamlInt(
      caml_ml_output_char(
        extract(env.instance, chanid),
        extract(env.instance, c)
      )
    );
  },
  caml_ml_flush: function(chanid) {
    return ocamlInt(caml_ml_flush(extract(env.instance, chanid)));
  },
  jsRaise_i32_i32: function() {
    throw new Error("Not implemented yet");
  },
  caml_lessequal: function() {
    throw new Error("Not implemented yet");
  },
  caml_greaterequal: function() {
    throw new Error("Not implemented yet");
  },
  caml_create_bytes: function(i) {
    return ocamlInt(caml_create_bytes(extract(env.instance, i)));
  },
  caml_blit_string: function(s1, i1, s2, i2, len) {
    return ocamlInt(
      caml_blit_string(
        extract(env.instance, s1),
        extract(env.instance, i1),
        extract(env.instance, s2),
        extract(env.instance, i2),
        extract(env.instance, len)
      )
    );
    throw new Error("Not implemented yet");
  },
  caml_format_int: function(fmt, i) {
    console.log("format int:", fmt, i);
    const result = caml_format_int(
      extract(env.instance, fmt),
      extract(env.instance, i)
    );
    return ocamlInt(result);
  },
  caml_int_of_string: function() {
    throw new Error("Not implemented yet");
  },
  jsRaise_i32_unit: function() {
    throw new Error("Not implemented yet");
  },
  caml_format_float: function(fmt, i) {
    const result = caml_format_float(
      extract(env.instance, fmt),
      extract(env.instance, i)
    );
    return ocamlInt(result);
  },
  caml_float_of_string: function() {
    throw new Error("Not implemented yet");
  },
  caml_sys_open: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_open_descriptor_out: function(fd) {
    return ocamlInt(caml_ml_open_descriptor_out(extract(env.instance, fd)));
  },
  caml_ml_set_channel_name: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_out_channels_list: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_output_bytes: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_output: function(chanid, buffer, offset, len) {
    console.log("caml_ml_output channel id:", extract(env.instance, chanid));

    return ocamlInt(
      caml_ml_output(
        extract(env.instance, chanid),
        extract(env.instance, buffer),
        extract(env.instance, offset),
        extract(env.instance, len)
      )
    );
  },
  caml_output_value: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_open_descriptor_in: function(fd) {
    return ocamlInt(caml_ml_open_descriptor_in(extract(env.instance, fd)));
  },
  caml_ml_input: function() {
    throw new Error("Not implemented yet");
  },
  caml_blit_bytes: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_input_scan_line: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_seek_out_64: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_pos_out_64: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_channel_size_64: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_seek_in_64: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_pos_in_64: function() {
    throw new Error("Not implemented yet");
  },
  caml_modify: function() {
    throw new Error("Not implemented yet");
  },
  caml_sys_exit: function() {
    throw new Error("Not implemented yet");
  },
  caml_register_named_value: function(name, value) {
    return ocamlInt(
      caml_register_named_value(
        extract(env.instance, name),
        extract(env.instance, value)
      )
    );
  },
  caml_fresh_oo_id: function() {
    return ocamlInt(caml_fresh_oo_id());
  },
  caml_int64_float_of_bits_unboxed: function(a, b) {
    return ocamlInt(a);
  },
  caml_compare: function() {
    throw new Error("Not implemented yet");
  },
  caml_int_compare: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_runtime_warnings_enabled: function() {
    throw new Error("Not implemented yet");
  },
  caml_ml_enable_runtime_warnings: function() {
    throw new Error("Not implemented yet");
  },
  caml_sys_getenv: function() {
    throw new Error("Not implemented yet");
  },
  caml_install_signal_handler: function() {
    throw new Error("Not implemented yet");
  },
  caml_sys_get_argv: function() {
    throw new Error("Not implemented yet");
  },
  caml_sys_get_config: function() {
    throw new Error("Not implemented yet");
  },
  caml_bytes_equal: function() {
    throw new Error("Not implemented yet");
  },
  caml_fill_bytes: function() {
    throw new Error("Not implemented yet");
  },
  caml_bytes_compare: function() {
    throw new Error("Not implemented yet");
  },
  caml_string_equal: function() {
    throw new Error("Not implemented yet");
  },
  caml_string_compare: function() {
    throw new Error("Not implemented yet");
  },
  caml_output_value_to_buffer: function() {
    throw new Error("Not implemented yet");
  },
  caml_marshal_data_size: function() {
    throw new Error("Not implemented yet");
  },
  caml_input_value_from_bytes: function() {
    throw new Error("Not implemented yet");
  },
  caml_floatarray_get: function() {
    throw new Error("Not implemented yet");
  },
  caml_floatarray_set: function() {
    throw new Error("Not implemented yet");
  },
  caml_output_value_to_bytes: function() {
    throw new Error("Not implemented yet");
  },
  caml_obj_tag: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_create: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_get_key: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_get_key_copy: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_set_key: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_unset_key: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_check_key: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_blit_key: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_get_data: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_get_data_copy: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_set_data: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_unset_data: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_check_data: function() {
    throw new Error("Not implemented yet");
  },
  caml_ephe_blit_data: function() {
    throw new Error("Not implemented yet");
  },
  caml_float_compare_unboxed: function() {
    throw new Error("Not implemented yet");
  },
  caml_hash: function() {
    throw new Error("Not implemented yet");
  },
  caml_array_concat: function() {
    throw new Error("Not implemented yet");
  },
  caml_make_float_vect: function() {
    throw new Error("Not implemented yet");
  },
  caml_make_vect: function() {
    throw new Error("Not implemented yet");
  },
  caml_array_sub: function() {
    throw new Error("Not implemented yet");
  },
  caml_array_append: function() {
    throw new Error("Not implemented yet");
  },
  caml_array_blit: function() {
    throw new Error("Not implemented yet");
  },
  caml_int32_format: function() {
    throw new Error("Not implemented yet");
  },
  caml_int32_of_string: function() {
    throw new Error("Not implemented yet");
  },
  caml_int32_compare_unboxed: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_add: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_sub: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_neg: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_xor: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_format: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_of_string: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_compare_unboxed: function() {
    throw new Error("Not implemented yet");
  },
  caml_nativeint_format: function() {
    throw new Error("Not implemented yet");
  },
  caml_nativeint_of_string: function() {
    throw new Error("Not implemented yet");
  },
  caml_nativeint_compare_unboxed: function() {
    throw new Error("Not implemented yet");
  },
  caml_lex_engine: function() {
    throw new Error("Not implemented yet");
  },
  caml_new_lex_engine: function() {
    throw new Error("Not implemented yet");
  },
  caml_set_parser_trace: function() {
    throw new Error("Not implemented yet");
  },
  caml_parse_engine: function() {
    throw new Error("Not implemented yet");
  },
  caml_notequal: function() {
    throw new Error("Not implemented yet");
  },
  caml_obj_set_tag: function() {
    throw new Error("Not implemented yet");
  },
  caml_obj_block: function() {
    throw new Error("Not implemented yet");
  },
  caml_lazy_make_forward: function() {
    throw new Error("Not implemented yet");
  },
  caml_hexstring_of_float: function() {
    throw new Error("Not implemented yet");
  },
  caml_classify_float_unboxed: function() {
    throw new Error("Not implemented yet");
  },
  caml_equal: function() {
    throw new Error("Not implemented yet");
  },
  caml_raw_backtrace_next_slot: function() {
    throw new Error("Not implemented yet");
  },
  caml_convert_raw_backtrace_slot: function() {
    throw new Error("Not implemented yet");
  },
  caml_raw_backtrace_slot: function() {
    throw new Error("Not implemented yet");
  },
  caml_raw_backtrace_length: function() {
    throw new Error("Not implemented yet");
  },
  caml_get_current_callstack: function() {
    throw new Error("Not implemented yet");
  },
  caml_get_exception_raw_backtrace: function() {
    throw new Error("Not implemented yet");
  },
  caml_backtrace_status: function() {
    throw new Error("Not implemented yet");
  },
  caml_record_backtrace: function() {
    throw new Error("Not implemented yet");
  },
  caml_convert_raw_backtrace: function() {
    throw new Error("Not implemented yet");
  },
  caml_apply6: function() {
    throw new Error("Not implemented yet");
  },
  caml_final_release: function() {
    throw new Error("Not implemented yet");
  },
  caml_final_register_called_without_value: function() {
    throw new Error("Not implemented yet");
  },
  caml_final_register: function() {
    throw new Error("Not implemented yet");
  },
  caml_gc_stat: function() {
    throw new Error("Not implemented yet");
  },
  caml_gc_counters: function() {
    throw new Error("Not implemented yet");
  },
  caml_md5_string: function() {
    throw new Error("Not implemented yet");
  },
  caml_md5_chan: function() {
    throw new Error("Not implemented yet");
  },
  caml_sys_random_seed: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_of_int: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_shift_left: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_or: function() {
    throw new Error("Not implemented yet");
  },
  caml_int64_mod: function() {
    throw new Error("Not implemented yet");
  },
  caml_greaterthan: function() {
    throw new Error("Not implemented yet");
  },
  caml_obj_dup: function() {
    throw new Error("Not implemented yet");
  },
  caml_hash_univ_param: function() {
    throw new Error("Not implemented yet");
  },
  caml_tuplify2: function() {
    throw new Error("Not implemented yet");
  },
  caml_weak_blit: function() {
    throw new Error("Not implemented yet");
  },
  caml_weak_check: function() {
    throw new Error("Not implemented yet");
  },
  caml_weak_get_copy: function() {
    throw new Error("Not implemented yet");
  },
  caml_weak_get: function() {
    throw new Error("Not implemented yet");
  },
  caml_weak_set: function() {
    throw new Error("Not implemented yet");
  },
  caml_weak_create: function() {
    throw new Error("Not implemented yet");
  },
  caml_obj_truncate: function() {
    throw new Error("Not implemented yet");
  },
  caml_lessthan: function() {
    throw new Error("Not implemented yet");
  },
  caml_set_oo_id: function() {
    throw new Error("Not implemented yet");
  },
  caml_make_array: function() {
    throw new Error("Not implemented yet");
  },
  caml_send0: function() {
    throw new Error("Not implemented yet");
  },
  caml_string_notequal: function() {
    throw new Error("Not implemented yet");
  },
  caml_sys_close: function() {
    throw new Error("Not implemented yet");
  },
  sqrt: function() {
    throw new Error("Not implemented yet");
  },
  atan2: function() {
    throw new Error("Not implemented yet");
  },
  cos: function() {
    throw new Error("Not implemented yet");
  },
  sin: function() {
    throw new Error("Not implemented yet");
  },
  exp: function() {
    throw new Error("Not implemented yet");
  },
  log: function() {
    throw new Error("Not implemented yet");
  },
  caml_spacetime_enabled: function() {
    throw new Error("Not implemented yet");
  },
  caml_register_channel_for_spacetime: function() {
    throw new Error("Not implemented yet");
  },
  caml_spacetime_write_magic_number: function() {
    throw new Error("Not implemented yet");
  },
  caml_spacetime_save_event: function() {
    throw new Error("Not implemented yet");
  },
  caml_spacetime_save_trie: function() {
    throw new Error("Not implemented yet");
  },
  caml_gc_minor: function() {
    throw new Error("Not implemented yet");
  },
  caml_spacetime_take_snapshot: function() {
    throw new Error("Not implemented yet");
  },
  caml_spacetime_save_event_for_automatic_snapshots: function() {
    throw new Error("Not implemented yet");
  }
};

let count = 0;
let globalInstance = null;
const importObject = {
  env,
  console: {
    log: function(e) {
      console.log("[ocaml-wasm] ", e);
    }
  },
  linking: {
    caml_curry2() {
      return 22;
    }
  },
  js: {
    tryWith: function(memPos, tryBody_, withHandler_) {
      const table = globalInstance.exports.table;
      const tryBody = table.get(tryBody_);
      const withHandler = table.get(withHandler_);
      try {
        tryBody(memPos);
      } catch (e) {
        withHandler(memPos, e);
      }
    },
    raise(e) {
      throw e;
    },
    caml_fresh_oo_id(v) {
      count++;
    }
  }
};

function fetchAndInstantiate(url) {
  return fetch(url)
    .then(response => response.arrayBuffer())
    .then(bytes => WebAssembly.instantiate(bytes, importObject))
    .then(results => results.instance);
}

function ocamlInt(i) {
  return (i << 1) + 1;
}

function jsInt(i) {
  if (i & (1 === 0)) throw Error("Expected an OCaml int, but got a pointer");
  return i >> 1;
}

describe("TEST", () => {
  describe("xxx", () => {
    it("should work", done => {
      fetchAndInstantiate("/base/test/curried_function.wasm")
        .then(instance => {
          console.error("THIS WORKS");
          done();
        })
        .catch(e => done(e));
    });
  });
});

const td = new TextDecoder("utf8");
describe("functions", () => {
  describe("noncurried function", () => {
    it("should return 20 when 5 is given", done => {
      fetchAndInstantiate("/base/test/noncurried_function.wasm")
        .then(instance => {
          const func =
            instance.exports.camlNoncurried_function__noncurried_function_1002;
          let calculatedValue = func(ocamlInt(5));
          expect(jsInt(calculatedValue)).to.equal(20);
          calculatedValue = func(ocamlInt(100));
          expect(jsInt(calculatedValue)).to.equal(115);
          done();
        })
        .catch(e => done(e));
    });
  });
  // describe("curried functions", () => {
  //   it("should return 30 when 6 and 20 is given - direct pointer", done => {
  //     fetchAndInstantiate("/base/test/curried_function.wasm")
  //       .then(instance => {
  //         // Object.keys(instance.exports).forEach(a => console.log(a));
  //         env.alloc = instance.exports.__heap_base.value;
  //         env.instance = instance;

  //         instance.exports._start();
  //         expect(
  //           extract(instance, instance.exports.camlCurried_function__bar_1002())
  //             .c
  //         ).to.equal(
  //           "Once upon a time there was a 2131232323 #@$!@#@#@ ^&%%^&%^& 1"
  //         );

  //         expect(extract(instance, convert("yolo1234")).c).to.equal("yolo1234");

  //         expect(
  //           extract(
  //             instance,
  //             instance.exports.camlCurried_function__hello_1005(
  //               convert("Foobar")
  //             )
  //           ).c
  //         ).to.equal("Foobar");

  //         // let func =
  //         //   instance.exports.camlCurried_function__curried_function_1639;
  //         // let calculatedValue = func(ocamlInt(6), ocamlInt(20));
  //         // expect(jsInt([calculatedValue])).to.equal(49);

  //         // func = instance.exports.camlCurried_function__curried_function_3_1643;
  //         // calculatedValue = func(ocamlInt(20));
  //         // expect(jsInt(calculatedValue)).to.equal(78);

  //         done();
  //       })
  //       .catch(e => done(e));
  //   });
  // });
  //
  //     // this function needs a change in wasmgen.ml - an extra argument needs to be added
  //     // to functions which have a fun_dbg list > 0
  //     it('should return 30 when 6 and 20 is given - caml_curry2', done => {
  //       fetchAndInstantiate("/base/test/curried_function.wasm").then(instance => {
  //         var i8 = new Uint8Array(instance.exports.memory.buffer);
  //         expect(i8[0]).to.equal(0);
  //         expect(instance.exports.caml_program()).to.equal(1);
  //         expect(i8[0]).to.equal(4);
  //         var i32_ = new Uint32Array(instance.exports.memory.buffer.slice(6, 14));
  //         expect(i32_[0]).to.equal(1792);
  //         expect(i32_[1]).to.equal(26);
  //
  //         // the pointer to caml_curry2
  //         var i32 = new Uint32Array(instance.exports.memory.buffer.slice(i32_[1] + 0, i32_[1] + 4));
  //         expect(i32[0]).to.equal(9);
  //
  //         // invoke caml_curry2
  //         let camlCurry2 = instance.exports.table.get(i32[0]);
  //         const allocatedMemoryAddress = camlCurry2(ocamlInt(6), i32_[1]);
  //         var i32 = new Uint32Array(instance.exports.memory.buffer.slice(allocatedMemoryAddress, allocatedMemoryAddress + 20));
  //
  //         // calculated value is a pointer to a allocated memory block
  //         // var i32 = new Uint32Array(instance.exports.memory.buffer.slice(allocatedMemoryAddress + 4, allocatedMemoryAddress + 20));
  //         let caml_curry2_1 = instance.exports.table.get(i32[0]);
  //
  //         const cv2 = caml_curry2_1(ocamlInt(20), allocatedMemoryAddress);
  //         expect(jsInt(cv2)).to.equal(30);
  //
  //         const cv3 = caml_curry2_1(ocamlInt(10), allocatedMemoryAddress + 4);
  //         expect(jsInt(cv3)).to.equal(20);
  //         done();
  //       })
  //       .catch(e => done(e))
  //     });
  //   });
  //
  //   // it('should support a simple crud', done => {
  //   //   fetchAndInstantiate("/base/test/crud.wasm").then(instance => {
  //   //     var i8 = new Uint8Array(instance.exports.memory.buffer);
  //   //     expect(i8[0]).to.equal(0);
  //   //     expect(instance.exports.caml_program()).to.equal(1);
  //   //     expect(i8[0]).to.equal(4);
  //   //     let dev = instance.exports.camlCrud__create_1211("Foo", 0);
  //   //     let dev2 = instance.exports.camlCrud__create_1211("Foo2", 0);
  //   //
  //   //
  //   //     done();
  //   //   })
  //   //   .catch(e => done(e))
  //   // });
  //
  //
  // });
  //
  //
  // describe('exception handling', () => {
  //   describe('raise + try with', () => {
  //     it ('should support basic exception handling', done => {
  //       fetchAndInstantiate("/base/test/exception_handling.wasm").then(instance => {
  //
  //         expect(instance.exports.caml_program()).to.equal(1);
  //
  //         globalInstance = instance;
  //         try {
  //           instance.exports.camlException_handling__other_1006();
  //         } catch (pointer) {
  //           var i32 = new Uint32Array(instance.exports.memory.buffer.slice(pointer + 0, pointer + 64));
  //           expect(i32[0]).to.equal(882);
  //           expect(i32[1]).to.equal(1);
  //         }
  //         try {
  //           instance.exports.camlException_handling__other2_1008();
  //         } catch (pointer) {
  //           var i32 = new Uint32Array(instance.exports.memory.buffer.slice(pointer + 0, pointer + 8));
  //           expect(i32[0]).to.equal(882);
  //           expect(i32[1]).to.equal(3);
  //         }
  //
  //         expect(instance.exports.camlException_handling__foo_1207(55)).to.equal(ocamlInt(500));
  //
  //         done();
  //       })
  //       .catch(e => { console.debug('what:', e); done(e) })
  //     })
  //   });
  // });
  //
  // describe('loop', () => {
  //   describe('loop', () => {
  //     it ('should loop', done => {
  //       fetchAndInstantiate("/base/test/loop.wasm").then(instance => {
  //         expect(instance.exports.caml_program()).to.equal(1);
  //         expect(instance.exports.camlLoop__a_1002(ocamlInt(3))).to.equal(ocamlInt(2950));
  //         done();
  //       })
  //       .catch(done)
  //     });
  //   });
  // });
  //
  // describe('switch', () => {
  //   describe('switch', () => {
  //     it ('should switch statements', done => {
  //       fetchAndInstantiate("/base/test/switch.wasm").then(instance => {
  //         expect(instance.exports.caml_program()).to.equal(1);
  //         expect(instance.exports.camlSwitch__small_switch_test1_1015()).to.equal(ocamlInt(222))
  //         expect(instance.exports.camlSwitch__small_switch_test2_1018()).to.equal(ocamlInt(70))
  //
  //         expect(instance.exports.camlSwitch__big_switch_test1_1021()).to.equal(ocamlInt(222))
  //         expect(instance.exports.camlSwitch__big_switch_test2_1024()).to.equal(ocamlInt(70));
  //         expect(instance.exports.camlSwitch__big_switch_test3_1027()).to.equal(ocamlInt(20));
  //         expect(instance.exports.camlSwitch__big_switch_test4_1030()).to.equal(ocamlInt(22));
  //
  //         expect(instance.exports.camlSwitch__big_switch_test5_1033()).to.equal(ocamlInt(30));
  //         expect(instance.exports.camlSwitch__big_switch_test6_1036()).to.equal(ocamlInt(70));
  //         expect(instance.exports.camlSwitch__big_switch_test7_1039()).to.equal(ocamlInt(40));
  //         expect(instance.exports.camlSwitch__big_switch_test8_1042()).to.equal(ocamlInt(70));
  //
  //         done();
  //       })
  //       .catch(e => { done(e) })
  //     });
  //   });
  // });
  //
  // describe('array', () => {
  //   describe('simple', () => {
  //     it ('should retrieve an array value', done => {
  //       fetchAndInstantiate("/base/test/array.wasm").then(instance => {
  //         expect(instance.exports.caml_program()).to.equal(1);
  //         expect(instance.exports.camlArray2__ala_1005()).to.equal(ocamlInt(10000000));
  //         expect(instance.exports.camlArray2__bla_1008()).to.equal(ocamlInt(2));
  //         try {
  //           instance.exports.camlArray2__cla_1011();
  //           done('should give an exception');
  //         }
  //         catch(pointer) {
  //           expect(pointer).to.equal(186);
  //         }
  //         try {
  //           instance.exports.camlArray2__dla_1014();
  //           done('should give an exception');
  //         }
  //         catch(pointer) {
  //           expect(pointer).to.equal(186);
  //         }
  //         done();
  //       })
  //       .catch(e => { done(e) })
  //     });
  //   });
  // });
  //
  //
  describe('arithmetic', () => {
    describe('int', () => {
      it ('should support basic arithmetic', done => {
        fetchAndInstantiate("/base/test/arithmetic.wasm").then(instance => {
          // expect(instance.exports.caml_program()).to.equal(1);
          expect(instance.exports.camlArithmetic__addi_1002(ocamlInt(5), ocamlInt(6))).to.equal(ocamlInt(11));
          expect(instance.exports.camlArithmetic__addi_1002(ocamlInt(5), ocamlInt(6))).to.equal(ocamlInt(11));

          expect(instance.exports.camlArithmetic__mini_1005(ocamlInt(10), ocamlInt(5))).to.equal(ocamlInt(5));
          expect(instance.exports.camlArithmetic__divi_1008(ocamlInt(10), ocamlInt(5))).to.equal(ocamlInt(2));
          expect(instance.exports.camlArithmetic__muli_1011(ocamlInt(10), ocamlInt(5))).to.equal(ocamlInt(50));

          expect(instance.exports.camlArithmetic__modi_1014(ocamlInt(10), ocamlInt(3))).to.equal(ocamlInt(1));
          expect(instance.exports.camlArithmetic__modi_1014(ocamlInt(99), ocamlInt(3))).to.equal(ocamlInt(0));
          expect(instance.exports.camlArithmetic__modi_1014(ocamlInt(101), ocamlInt(3))).to.equal(ocamlInt(2));

          expect(instance.exports.camlArithmetic__land__1017(ocamlInt(10), ocamlInt(3))).to.equal(ocamlInt(2));
          expect(instance.exports.camlArithmetic__lor__1020(ocamlInt(4), ocamlInt(2))).to.equal(ocamlInt(6));
          expect(instance.exports.camlArithmetic__lxor__1023(ocamlInt(10), ocamlInt(3))).to.equal(ocamlInt(9));
          expect(instance.exports.camlArithmetic__lsl__1026(ocamlInt(10), ocamlInt(1))).to.equal(ocamlInt(20));
          expect(instance.exports.camlArithmetic__lsr__1029(ocamlInt(10), ocamlInt(1))).to.equal(ocamlInt(5));
          expect(instance.exports.camlArithmetic__asr__1032(ocamlInt(10), ocamlInt(1))).to.equal(ocamlInt(5));
          done();
        })
        .catch(e => { done(e) })
      })
    });
    // describe('float', () => {
    //   xit ('should support basic arithmetic', done => {
    //     fetchAndInstantiate("/base/test/arithmetic.wasm").then(instance => {
    //       /* floats work, but tests need to be fixed - need to figure how one can access the memory from js */
    //       //
    //       //
    //       //
    //       // expect(instance.exports.caml_program()).to.equal(1);
    //       //
    //       // let alloc = instance.exports.table.get(3);
    //       // let addr = alloc(8);
    //       //
    //       // const x = new Float32Array(instance.exports.memory.buffer.slice(addr, addr + 12));
    //       // x[0] = 5;
    //       // x[1] = 12;
    //       //
    //       // instance.exports.memory
    //       // const x21 = new Float32Array(instance.exports.memory.buffer.slice(addr, addr + 12));
    //       // console.debug('riiight1:', x21[0], x21[1], x21[2], x[0], x[1]);
    //       //
    //       // var pointer = instance.exports.camlArithmetic__divf_1043(addr, addr + 4);
    //       // const x2 = new Float32Array(instance.exports.memory.buffer.slice(pointer, pointer + 4));
    //       // console.debug('riiight:', x2[0]);

    //       done();
    //     })
    //       .catch(e => { done(e) })
    //   })
    // });
  })
});
