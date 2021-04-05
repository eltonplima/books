from pydantic import BaseModel


class Account(BaseModel):
    debits: int
    credits: int
    fees: int

    @property
    def balance(self) -> int:
        return self.credits - self.debits


def format_amount(value: int) -> str:
    result = f"{value}"
    if value < 0:
        return result + "-"
    return result


def print_balance(account):
    print(f"Debits: {format_amount(account.debits)}")
    print(f"Credits: {format_amount(account.credits)}")
    print(f"Fees: {format_amount(account.fees)}")
    print("-" * 80)
    print(f"Balance: {format_amount(account.balance)}")
