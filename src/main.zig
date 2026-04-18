const std = @import("std");
const Buffer = std.ArrayList;
const Map = std.StringHashMap;

const TOKEN = u64;
const open_quote = '(';
const close_quote = ')';
const open_word = ':';
const close_word = ';';
const iden = 0;
const number = 1;

const Word = u16;

const Token = struct {
	tag: TOKEN,
	value: union(enum){
		text: []const u8,
		numeric: Word
	}
};

pub fn tokenize(mem: *const std.mem.Allocator, text: []const u8) Buffer(Token) {
	var tokens = Buffer(Token).init(mem.*);
	var i: u64 = 09;
	while (i < text.len){
		const c = text[i];
		switch(c) {
			' ', '\t', '\n', '\r' => {
				i += 1;
				continue;
			},
			open_quote, close_quote, open_word, close_word => {
				tokens.append(Token{
					.tag = c,
					.value = .{
						.text = text[i..i+1]
					}
				}) catch unreachable;
				i += 1;
				continue;
			},
			else => {
				if (std.ascii.isDigit(c)){
					i += 1;
					var value = c-48;
					if (text[i] == 'x'){
						i += 1;
						while (std.ascii.isHex(text[i]) and i < text.len){
							value *= 16;
							if (std.ascii.isDigit(text[i])){
								value += text[i]-48;
							}
							else if (std.ascii.isLower(text[i])){
								value += (text[i]-97)+10;
							}
							else {
								value += (text[i]-65)+10;
							}
							i += 1;
						}
					}
					else{
						while (std.ascii.isDigit(text[i]) and i < text.len){
							value *= 10;
							value += text[i]-48;
							i += 1;
						}
					}
					tokens.append(Token{
						.tag = number,
						.value = .{
							.numeric = value
						}
					});
					continue;
				}
				else if (std.ascii.isAlphanumeric(c) or c == '_'){
					const start = i;
					while ((std.ascii.isAlphanumeric(c) or c == '_') and i < text.len){
						i += 1;
					}
					tokens.append(Token{
						.tag = iden,
						.value = .{
							.text = text[start .. i]
						}
					}) catch unreachable;
				}
			}
		}
		i += 1;
	}
	return tokens;
}

const Inst = union(enum){
	psh_ds,
	pop_ds,
	psh_rs,
	pop_rs,
	psh_cs,
	pop_cs,
	read_ds,
	write_ds,
	jmp,
	nip,
	rot,
	dup,
	cut,
	ovr,
	data: Word
};

const ParseError = error {
	UnexpectedToken,
};

pub fn parse(mem: *const std.mem.Allocator, tokens: []Token, instructions: *Buffer(Inst), close_token: ?TOKEN) ParseError!void {
	var i: u64 = 0;
	var defs = Map(Word).init(mem.*);
	var def_backlog = Map(Buffer(u64)).init(mem.*);
	while (i < tokens.len){
		switch (tokens[i].tag){
			open_quote => {
				instructions.append(Inst{.jmp=undefined}) catch unreachable;
				const save = instructions.items.len;
				instructions.append(Inst{.data=0}) catch unreachable;
				try parse(mem, tokens, instructions, close_quote);
				instructions.append(Inst{ .psh_rt=undefined}) catch unreachable;
				instructions.items[save].data = instructions.items.len*2;
				i += 1;
				continue;
			},
			close_quote => {
				if (close_token)|end|{
					if (end == close_quote){
						return;
					}
				}
				return ParseError.UnexpectedToken;
			},
			open_word => {
				i += 1;
				const loc = instructions.items.len*2;
				const name = tokens[i];
				if (name.tag != iden){
					return ParseError.UnexpectedToken;
				}
				instructions.append(Inst{.jmp=undefined}) catch unreachable;
				const save = instructions.items.len;
				instructions.append(Inst{.data=0}) catch unreachable;
				try parse(mem, tokens, instructions, close_word);
				instructions.append(Inst{ .pop_rt=undefined}) catch unreachable;
				defs.put(name.value.text, loc) catch unreachable;
				instructions.items[save].jump = instructions.items.len*2;
				if (def_backlog.get(name.value.text)) |list| {
					for (list.items) |index| {
						instructions.items[index].data = loc;
					}
				}
				i + 1;
				continue;
			},
			close_word => {
				if (close_token)|end|{
					if (end == close_word){
						return;
					}
				}
				return ParseError.UnexpectedToken;
			},
			iden => {
				if (std.mem.eql(u8, tokens[i].value.text, "nip")){
					instructions.append(Inst{ .nip = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "ovr")){
					instructions.append(Inst{ .ovr = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "rot")){
					instructions.append(Inst{ .rot = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "dup")){
					instructions.append(Inst{ .dup = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "cut")){
					instructions.append(Inst{ .cut = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "pop")){
					instructions.append(Inst{ .pop_ds = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "st")){
					instructions.append(Inst{ .write_ds = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "ld")){
					instructions.append(Inst{ .read_ds = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (defs.get(tokens[i].value.text)) |address| {
					instructions.append(Inst{ .psh_rt = undefined}) catch unreachable;
					instructions.append(Inst{ .jmp=undefined, }) catch unreachable;
					instructions.append(Inst{ .data = address}) catch unreachable;
					i += 1;
					continue;
				}
				else{
					instructions.append(Inst{ .psh_rt = undefined}) catch unreachable;
					instructions.append(Inst{ .jmp = undefined, }) catch unreachable;
					instructions.append(Inst{ .data = 0}) catch unreachable;
					if (def_backlog.getPtr(tokens[i].value.text)) |list| {
						list.append(instructions.items.len-1) catch unreachable;
					}
					else {
						var list = Buffer(Word).init(mem.*);
						list.append(instructions.items.len-1) catch unreachable;
						def_backlog.put(tokens[i].value.text, list) catch unreachable;
					}
					i += 1;
					continue;
				}
			},
			number => {
				instructions.append(Inst{ .psh_ds = undefined}) catch unreachable;
				instructions.append(Inst{ .data = tokens[i].value.numeric }) catch unreachable;
				i += 1;
				continue;
			}
		}
	}
}

const OPCODE = Word;
const NOP = 0;
const PSH_DS = 1;
const POP_DS = 2;
const PSH_RS = 3;
const POP_RS = 4;
const PSH_CS = 5;
const POP_CS = 6;
const LD = 7;
const ST = 8;
const JMP = 9;
const NIP = 10;
const ROT = 11;
const DUP = 12;
const CUT = 13;
const OVR = 14;

pub fn code_gen(mem: *const std.mem.Allocator, instructions: Buffer(Inst)) []u8 {
	var bytes = mem.alloc(u8, instructions.items.len*2) catch unreachable;
	var i: u64 = 0;
	for (instructions.items) |inst| {
		switch (inst){
			.psh_ds => {bytes[i] = PSH_DS;},
			.pop_ds => {bytes[i] = POP_DS;},
			.psh_rs => {bytes[i] = PSH_RS;},
			.pop_rs => {bytes[i] = POP_RS;},
			.psh_cs => {bytes[i] = PSH_CS;},
			.pop_cs => {bytes[i] = POP_CS;},
			.read_ds => {bytes[i] = LD;},
			.write_ds => {bytes[i] = ST;},
			.jmp => {bytes[i] = JMP;},
			.nip => {bytes[i] = NIP;},
			.rot => {bytes[i] = ROT;},
			.dup => {bytes[i] = DUP;},
			.cut => {bytes[i] = CUT;},
			.ovr => {bytes[i] = OVR;},
			.data => {bytes[i] = (inst.data & 0xFF00) >> 8;}
		}
	}
	return bytes;
}

pub fn main() !void {
	const heap = std.heap.page_allocator;
	const main_buffer = heap.alloc(u8, 0x10000) catch unreachable;
	const temp_buffer = heap.alloc(u8, 0x10000) catch unreachable;
	var main_mem_fixed = std.heap.FixedBufferAllocator.init(main_buffer);
	var temp_mem_fixed = std.heap.FixedBufferAllocator.init(temp_buffer);
	var main_mem = main_mem_fixed.allocator();
	var temp_mem = temp_mem_fixed.allocator();
	const args = try std.process.argsAlloc(main_mem);
	if (args.len == 1){
		std.debug.print("-h for help\n", .{});
		return;
	}
	if (std.mem.eql(u8, args[1], "-h")){
		std.debug.print("Help Menu\n", .{});
		std.debug.print("   -h : Show this message\n", .{});
		std.debug.print("   [filename] : evaluate file\n", .{});
		return;
	}
	if (std.mem.eql(u8, args[1], "-i")){
		idle(&main_mem, &temp_mem, temp_mem_fixed);
		return;
	}
	const filename = args[1];
	const contents = try get_contents(&main_mem, filename);
	const tokens = tokenize(&main_mem, contents);
	const instructions = Buffer(Inst).init(main_mem);
	parse(&main_mem, tokens.items, instructions, null);
	const bytes = code_gen(&main_mem, instructions);
}
