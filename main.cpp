#include <iostream>
#include <vector>
#include <set>
#include <string>
#include <cassert>
#include <fstream>
#include <functional>

#define debug(a) std::cout << a << std::endl;

std::vector<unsigned char> huffman_encode(std::string & s) {
	if (s.empty()) return std::vector<unsigned char>();
	std::vector<std::pair<int, int> > g;
	std::vector<unsigned int> cnt(256, 0);
	for (int i = 0; i < s.size(); ++i)
		++cnt[(unsigned long) s[i]];
	std::set<std::pair<int, int> > st;
	std::vector<char> symbol;
	symbol.reserve(256);
	for (int i = 0; i < 256; ++i) {
		if (cnt[i]) {
			st.insert(std::make_pair(cnt[i], g.size()));
			symbol.push_back(i);
			g.push_back(std::make_pair(-1, -1));
		}
	}
	if (st.size() == 1) { // one symbol repeated
		std::vector<unsigned char> ans;
		char c = symbol[st.begin()->second];
		ans.push_back(0);
		ans.push_back(0);
		ans.push_back(0);
		ans.push_back(0);
		ans.push_back(c);

		ans.push_back((cnt[c] >> 24) & ((1 << 8) - 1));
		ans.push_back((cnt[c] >> 16) & ((1 << 8) - 1));
		ans.push_back((cnt[c] >> 8) & ((1 << 8) - 1));
		ans.push_back(cnt[c] & ((1 << 8) - 1));
		return ans;
	}
	while (st.size() > 1) {
		std::pair<int, int> f = *st.begin();
		st.erase(st.begin());
		std::pair<int, int> s = *st.begin();
		st.erase(st.begin());
		st.insert(std::make_pair(f.first + s.first, g.size()));
		g.push_back(std::make_pair(f.second, s.second));
	}
	int root = st.begin()->second;
	std::vector<std::vector<bool> > ws(256);
	std::vector<bool> path;
	std::vector<char> sls;
	std::vector<unsigned char> ans;
	unsigned int length = 0;
	std::function<void(int)> dfs = [&g, &ws, &path, &ans, &symbol, &length, &cnt, &dfs, &sls](int v) {
		ans.push_back((v >> 8) & ((1 << 8) - 1));
		ans.push_back(v & ((1 << 8) - 1));
		if (g[v].first == -1) { // leaf
			ws[symbol[v]] = path;
			length += cnt[symbol[v]] * path.size();
			sls.push_back(symbol[v]);
		} else {
			path.push_back(false);
			dfs(g[v].first);
			ans.push_back((v >> 8) & ((1 << 8) - 1));
			ans.push_back(v & ((1 << 8) - 1));
			path.back() = true;
			dfs(g[v].second);
			ans.push_back((v >> 8) & ((1 << 8) - 1));
			ans.push_back(v & ((1 << 8) - 1));
			path.pop_back();
		}
	};
	dfs(root);
	ans.push_back((root >> 8) & ((1 << 8) - 1));
	ans.push_back(root & ((1 << 8) - 1));
	for (char c : sls) ans.push_back(c);
	ans.push_back((length >> 24) & ((1 << 8) - 1));
	ans.push_back((length >> 16) & ((1 << 8) - 1));
	ans.push_back((length >> 8) & ((1 << 8) - 1));
	ans.push_back(length & ((1 << 8) - 1));
	char count = 8;
	for (unsigned char c : s) {
		for (bool b : ws[c]) {
			if (count == 8) {
				count = 0;
				ans.push_back(0);
			}
			ans.back() <<= 1;
			if (b)
				ans.back() |= 1;
			++count;
		}
	}
	ans.back() <<= 8 - count;
	return ans;
}

std::string huffman_decode(std::vector<unsigned char> &in) {
	if (in.size() < 2) return std::string();
	int i = 3;
	int root = (((short int)in[0]) << 8) | in[1];
	std::vector<int> stack(1, root);
	std::vector<std::pair<int, int> > g(root+1); // root is size-1, check encode code to understand why that is
	g[root] = std::make_pair(-1, -1);
	std::vector<char> symbols(root+1);
	bool fl = false;
	for (; i < in.size(); i += 2) {
		int v = (((short int)in[i-1]) << 8) | in[i];
		if (stack.size() == 1 && v == root) {
			++i;
			fl = true;
			break;
		}
		if (stack.size() > 1 && stack[stack.size() - 2] == v) {
			stack.pop_back();
		} else {
			if (g[stack.back()].first == -1)
				g[stack.back()].first = v;
			else
				g[stack.back()].second = v;
			stack.push_back(v);
			g[v] = std::make_pair(-1, -1);
		}
	}
	assert(fl);
	if (g.size() == 1) {
		// one symbol repeated
		char c = in[i++];
		std::string ans;
		unsigned int length = ((unsigned int) in[i + 3]) | (((unsigned int) in[i + 2]) << 8) | (((unsigned int) in[i + 1]) << 16) | (((unsigned int) in[i]) << 24);
		ans.assign(length, c);
		return ans;
	}
	std::vector<int> symbol_list;
	std::function<void(int)> dfs = [&g, &symbol_list, &dfs](int v) {
		if (g[v].first == -1) {
			symbol_list.push_back(v);
		} else {
			dfs(g[v].first);
			assert(g[v].second != -1);
			dfs(g[v].second);
		}
	};
	dfs(root);
	for (int j = 0; i < in.size() && j < symbol_list.size(); ++i, ++j)
		symbols[symbol_list[j]] = in[i];
	unsigned int count = 0, j = 0,
		length = ((unsigned int) in[i + 3]) | (((unsigned int) in[i + 2]) << 8) | ((unsigned int) in[i + 1] << 16) | (((unsigned int) in[i]) << 24);
	i += 4;
	char now = in[i++];
	std::string ans;
	int state = root;
	for (; j < length; ++j, ++count) {
		if (count == 8) {
			count = 0;
			now = in[i++];
		}
		bool wh = (now >> (7 - count)) & 1;
		if (wh) state = g[state].second;
		else state = g[state].first;
		if (g[state].first == -1) {
			ans.push_back(symbols[state]);
			state = root;
		}
	}
	return ans;
}

void huffman_encode_to_file(std::string &file_path, std::string &s) {
	std::ofstream output(file_path, std::ios::binary);
	if (output.is_open()) {
		std::vector<unsigned char> out = huffman_encode(s);
		output.write((char*)&out[0], out.size() * sizeof(unsigned char));
		output.close();
	} else {
		throw std::invalid_argument("File cannot be opened");
	}
}

std::string huffman_decode_from_file(std::string &file_path) {
	std::ifstream input(file_path, std::ios::binary);
	if (input.is_open()) {
		input.seekg(0, std::ios::end);
		std::vector<unsigned char> in;
		in.reserve(input.tellg());
		input.unsetf(std::ios::skipws);
		input.seekg(0, std::ios::beg);
		in.insert(in.begin(), std::istream_iterator<unsigned char>(input), std::istream_iterator<unsigned char>());
		input.close();
		return huffman_decode(in);
	} else {
		throw std::invalid_argument("File not found or cannot be opened");
	}
}

void huffman_encode_file(std::string &input_file, std::string &output_file) {
	std::ifstream input(input_file);
	if (input.is_open()) {
		input.seekg(0, std::ios::end);
		std::string in;
		in.resize((unsigned long) input.tellg());
		input.seekg(0, std::ios::beg);
		input.read(&in[0], in.size());
		input.close();
		huffman_encode_to_file(output_file, in);
	} else {
		throw std::invalid_argument("File not found or cannot be opened");
	}
}

void huffman_decode_file(std::string &input_file, std::string &output_file) {
	std::string text = huffman_decode_from_file(input_file);
	std::ofstream output(output_file);
	if (output.is_open()) {
		output << text;
		output.close();
	} else {
		throw std::invalid_argument("File not found or cannot be opened");
	}
}

int main() {
	std::string input = "451.txt";
	std::string output = "451.huf";
	huffman_encode_file(input, output);
  std::string out_test = "451_2.txt";
  huffman_decode_file(output, out_test);
}
