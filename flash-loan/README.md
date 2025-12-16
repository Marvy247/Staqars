# Flash Loan - Uncollateralized Instant Loans

Borrow any amount without collateral, as long as you repay within the same transaction.

## Features

- **Zero Collateral**: Borrow without posting collateral
- **Instant Execution**: Borrow and repay in one transaction
- **Liquidity Pools**: Provide liquidity and earn fees
- **Flash Loan Fee**: 0.09% fee on borrowed amount
- **Profit Tracking**: Track arbitrage profits from flash loans
- **Safety Checks**: Automatic revert if not repaid

## Key Functions

### add-liquidity
Provide liquidity to the pool and earn fees.

### remove-liquidity
Withdraw your liquidity when no loans active.

### execute-flash-loan
Borrow funds for arbitrage or other strategies.

### repay-flash-loan
Repay the loan plus fee in same transaction.

### claim-fees
Claim earned fees from providing liquidity.

## Use Cases

- Arbitrage trading
- Collateral swapping
- Liquidation assistance
- Capital efficiency
- DeFi composability

## How It Works

1. Borrow funds instantly
2. Use funds for profitable operation
3. Repay loan + fee in same transaction
4. Keep the profit

If repayment fails, entire transaction reverts.

## Deployment

```bash
clarinet check
clarinet deploy --testnet
```
