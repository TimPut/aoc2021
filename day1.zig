const std = @import("std");
const os = std.os;
const warn = std.debug.warn;

pub fn main() !void {
  var file = try std.fs.cwd().openFile("./day1input.txt", .{});
  defer file.close();

  var buf_reader = std.io.bufferedReader(file.reader());
  var in_stream = buf_reader.reader();

  const out_file = std.io.getStdOut();

  var depths: [2000]u16 = undefined;
  var buf: [1024]u8 = undefined;
  var li: u16 = 0;
  while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
    depths[li] = try std.fmt.parseInt(u16, line, 10);
    li += 1;
  }

  // Part 1
  var count : u16 = 0;
  for (depths[1..]) |d,i| {
    if (d > depths[i]) count += 1;
    }
  try out_file.writer().print(
      "Part one: {d}\n",
      .{count},
  );

  // Part 2
  count = 0;
  for (depths[3..]) |_,i| {
    if (depths[i+3] > depths[i]) count += 1;
    }
  try out_file.writer().print(
      "Part two: {d}\n",
      .{count},
  );
}
