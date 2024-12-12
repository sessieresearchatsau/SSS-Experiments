def test_list_comps(n: int) -> None:
    for i in range(1, n+1):
        # The -1 lower limit is there to imitate Mathematica's
        # 9-0 sequence for this same representation
        print([j for j in range(n, -1, -i)])
        
if __name__ == "__main__":
    test_list_comps(10)