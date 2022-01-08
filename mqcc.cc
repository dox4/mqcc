#include "mqcc.h"
#include "ast.h"
#include "error.h"
#include "generator.h"
#include "parser.h"
#include "scanner.h"
#include "token.h"
#include <array>
#include <bits/getopt_core.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <ios>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <string_view>

#include <cstdio>
#include <unistd.h>

using namespace std;

string read_file(const char *file_name) {
    ifstream ifs(file_name);
    string ret((istreambuf_iterator<char>(ifs)), (istreambuf_iterator<char>()));
    ifs.close();
    return ret;
}

struct CmdOpt {
    const char *input_file  = nullptr;
    const char *output_file = nullptr;
    bool dump_ast           = false;
};

CmdOpt parse_opt(int argc, char **argv) {
    auto fmt = "o:f:";
    int c;
    CmdOpt ret;
    while ((c = getopt(argc, argv, fmt)) != -1) {
        switch (c) {
        case 'o':
            ret.output_file = optarg;
            break;
        case 'f':
            if (strcmp(optarg, "dumpast") == 0)
                ret.dump_ast = true;
            else {
                fprintf(stderr, "unknown value for option `f`: %s\n", optarg);
                exit(1);
            }
            break;
        default:
            cout << "default: " << optarg << endl;
        }
    }
    if (ret.output_file == nullptr)
        ret.output_file = "a.out";
    ret.input_file = argv[optind];
    return ret;
}

static const char *write_asm(const string &code) {
    static const char *tmp_file_name = "/tmp/mqcc.s";

    ofstream ofs;
    ofs.open(tmp_file_name, ios::out);
    ofs << code;
    ofs.close();
    return tmp_file_name;
}

static void run_gcc(const char *asm_file, const char *out_file) {
    stringstream cmd;
    cmd << "gcc -std=c11";
    cmd << " " << asm_file;
    cmd << " "
        << "-o"
        << " " << out_file;
    system(cmd.str().c_str());
}

int main(int argc, char **argv) {
    if (argc < 2) {
        cerr << "usage: mqcc <file-name>" << endl;
        return 1;
    }

    auto opt = parse_opt(argc, argv);

    auto src = read_file(opt.input_file);

    Scanner scanner(opt.input_file, src);
    Parser parser(&scanner);
    auto unit = parser.parse();
    Generator g(unit, parser.scope());
    g.gen();
    auto code = g.code();

    auto tmp = write_asm(code);
    // set_mark();

    run_gcc(tmp, opt.output_file);

    return 0;
}
