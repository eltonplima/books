from pydantic import BaseModel


class Account(BaseModel):
    debits: int
    credits: int
    fees: int

    @property
    def balance(self) -> int:
        return self.credits - self.debits


def print_balance(account):
    print(f"Debits: {account.debits}")
    print(f"Credits: {account.credits}")
    if account.fees < 0:
        print(f"Fees: {account.fees}-")
    else:
        print(f"Fees: {account.fees}")
    print("-" * 80)
    if account.balance < 0:
        print(f"Balance: {account.balance}-")
    else:
        print(f"Balance: {account.balance}")
