from chapter2.duplication_in_code import print_balance, Account


def test_print_balance__with_positive_empty_balance(capsys):
    print_balance(Account(credits=10, debits=10, fees=0))
    assert capsys.readouterr().out == """Debits: 10
Credits: 10
Fees: 0
--------------------------------------------------------------------------------
Balance: 0
"""


def test_print_balance__with_negative_empty_balance(capsys):
    print_balance(Account(credits=10, debits=20, fees=0))
    assert capsys.readouterr().out == """Debits: 20
Credits: 10
Fees: 0
--------------------------------------------------------------------------------
Balance: -10-
"""
