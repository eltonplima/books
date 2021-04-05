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


def print_line(label: str, value: str):
    print(f"{label} {value}")


def report_line(label: str, amount: int):
    print_line(label + ":", format_amount(amount))


def print_balance(account):
    report_line("Debits", account.debits)
    report_line("Credits", account.credits)
    report_line("Fees", account.fees)
    print("-" * 80)
    report_line("Balance", account.balance)
