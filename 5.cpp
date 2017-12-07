#include <fstream>
#include <functional>
#include <iostream>
#include <string>
#include <vector>

static std::vector<int> parse( const std::string &fileName )
{
    std::vector<int> memory;
    std::ifstream f ("5.input" );
    std::string line;
    while ( std::getline( f, line ) ) {
        memory.push_back( std::stoi( line ) );
    }
    return memory;
}

static int run( std::vector<int> v, std::function<int(int)> adjust )
{
    unsigned int nsteps = 0;
    int pos = 0;
    while ( pos >= 0 && pos < v.size() ) {
        int newPos = pos + v[pos];
        v[pos] = adjust( v[pos] );
        pos = newPos;
        ++nsteps;
    }
    return nsteps;
}

int main()
{
    std::vector<int> memory = parse( "5.input" );
    std::cout << run( memory, [](int x) { return x + 1; } ) << '\n';
    std::cout << run( memory, [](int x) { return x >= 3 ? x - 1 : x + 1; } ) << '\n';
}


