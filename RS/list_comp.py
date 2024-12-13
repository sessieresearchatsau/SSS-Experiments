def test_list_comps(n: int) -> None:
    for i in range(1, n+1):
        # The -1 lower limit is there to imitate Mathematica's
        # 9-0 sequence for this same representation
        print([j for j in range(n, -1, -i)])
        
def dec_pattern_1(n: int) -> None:
    # The form ICN currently takes
    print([[n-j-i for j in range(0, n-i)] for i in range(0, n)])
    # Using reverse iterators
    print([[j for j in range(i, 0, -1)] for i in range(n, 0, -1)])
    
if __name__ == "__main__":
    test_list_comps(10)
    dec_pattern_1(3)