# Digital Will - Time-Locked Inheritance Contract

**Built for Stacks Builder Challenge by Marcus David**

## Deployed Contract

**Testnet Address:** `ST3P3DPDB69YP0Z259SS6MSA16GBQEBF8KG8P96D2.digital-will`

**Explorer:** [View on Stacks Explorer](https://explorer.hiro.so/txid/ST3P3DPDB69YP0Z259SS6MSA16GBQEBF8KG8P96D2.digital-will?chain=testnet)

**Deployer:** `ST3P3DPDB69YP0Z259SS6MSA16GBQEBF8KG8P96D2`

##  Overview
A decentralized inheritance contract that automatically transfers STX to beneficiaries after a period of owner inactivity. This project showcases **Clarity 4's block-timestamp functionality** (using block-height as a proxy in current implementation).

##  Key Features
- **Time-based inheritance**: Automatically transferable after inactivity period
- **Secure escrow**: STX locked in contract until conditions are met
- **Activity tracking**: Owners can check-in to reset inactivity timer
- **Multiple wills**: Support for up to 10 wills per user
- **Clarity 4**: Demonstrates time-tracking patterns for Bitcoin L2

##  Use Cases
- Estate planning on Bitcoin
- Dead man's switch for crypto assets
- Time-locked asset distribution
- Automated inheritance without intermediaries

##  Contract Functions

### Public Functions
- `create-will(beneficiary, amount)` - Lock STX for a beneficiary
- `update-activity(will-id)` - Reset inactivity timer (owner check-in)
- `claim-inheritance(will-id)` - Beneficiary claims after inactivity period
- `cancel-will(will-id)` - Owner cancels before claim

### Read-Only Functions
- `get-will(will-id)` - Get will details
- `get-user-wills(user)` - List all wills for a user
- `can-claim(will-id)` - Check if will is claimable
- `get-current-timestamp()` - Get current block height

##  How It Works

1. **Create Will**: Owner deposits STX and designates a beneficiary
2. **Activity Tracking**: Contract tracks last activity timestamp
3. **Inactivity Period**: If owner doesn't check-in for 180 days (~25,920 blocks)
4. **Inheritance**: Beneficiary can claim the funds

##  Deployment

```bash
# Check contract
clarinet check

# Test locally
clarinet test

# Deploy to testnet
clarinet deploy --testnet

# Deploy to mainnet
clarinet deploy --mainnet
```

##  Clarity 4 Features

This contract demonstrates:
- **Time-based logic** using block-height (Clarity 4 uses `block-timestamp`)
- **Automatic asset distribution** based on inactivity
- **Decentralized inheritance** without intermediaries

##  Built For
-  Stacks Builder Challenge (Dec 10-14, 2024)
-  Demonstrating advanced Clarity smart contract patterns
-  Building reputation in Stacks ecosystem

##  License
MIT License - Open source for educational purposes

##  Author
Marcus David - [@MarcusDavid](https://github.com/yourusername)
