#import "@preview/cetz:0.2.2"

#let image-background = image("./images/background-1.jpg", height: 100%, fit: "cover")
#let image-foreground = image("./images/Logo-Anastasia-Labs-V-Color02.png", width: 100%, fit: "contain")
#let image-header = image("./images/Logo-Anastasia-Labs-V-Color01.png", height: 75%, fit: "contain")
#let fund-link = link("https://projectcatalyst.io/funds/10/f10-osde-open-source-dev-ecosystem/anastasia-labs-the-trifecta-of-data-structures-merkle-trees-tries-and-linked-lists-for-cutting-edge-contracts")[Catalyst Proposal]
#let git-link = link("https://github.com/Anastasia-Labs/data-structures")[Main Github Repo]

#set page(
  background: image-background,
  paper :"a4",
  margin: (left : 20mm,right : 20mm,top : 40mm,bottom : 30mm)
)

// Set default text style
#set text(15pt, font: "Montserrat")

#v(3cm) // Add vertical space

#align(center)[
  #box(
    width: 60%,
    stroke: none,
    image-foreground,
  )
]

#v(1cm) // Add vertical space

// Set text style for the report title
#set text(20pt, fill: white)

// Center-align the report title
#align(center)[#strong[Midgard]]
#align(center)[#strong[Non-technical Documentation]]

#v(5cm)

// Set text style for project details
#set text(13pt, fill: white)


// Reset text style to default
#set text(fill: luma(0%))

// Display project details
#show link: underline
#set terms(separator:[: ],hanging-indent: 18mm)

#set par(justify: true)
#set page(
  paper: "a4",
  margin: (left: 20mm, right: 20mm, top: 40mm, bottom: 35mm),
  background: none,
  header: [
    #align(right)[
      #image("./images/Logo-Anastasia-Labs-V-Color01.png", width: 25%, fit: "contain")
    ]
    #v(-0.5cm)
    #line(length: 100%, stroke: 0.5pt)
  ],
)

#v(20mm)
#show link: underline
#show outline.entry.where(level: 1): it => {
  v(6mm, weak: true)
  strong(it)
}

#outline(depth:2, indent: 1em)
#pagebreak()
#set text(size: 11pt)  // Reset text size to 10pt
#set page(
   footer: [
    #line(length: 100%, stroke: 0.5pt)
    #v(-3mm)
    #align(center)[ 
      #set text(size: 11pt, fill: black)
      *Anastasia Labs – *
      #set text(size: 11pt, fill: gray)
      *Midgard*
      #v(-3mm)
      Non-technical Documentation
      #v(-3mm)
    ]
    #v(-6mm)
    #align(right)[
      #context counter(page).display( "1/1",both: true)]
  ] 
)

// Initialize page counter
#counter(page).update(1)
#v(100pt)
// Display project details
// #set terms(separator:[: ],hanging-indent: 18mm)
// #align(center)[
//   #set text(size: 20pt)
//   #strong[Midgard]]
// #v(20pt)
\

#set heading(numbering: "1.")
#show heading: set text(rgb("#c41112"))

= What is Midgard?
\

*Midgard* is an innovative *layer 2 scaling solution* built on top of the Cardano blockchain. 

It uses a *custom-built optimistic rollup* technology to help Cardano process more transactions, at lower cost, while keeping the network secure.

*Optimistic rollups* assume transactions are valid unless proven otherwise, allowing for faster and more efficient operations.

By doing this, Midgard ensures that as Cardano grows, it stays fast, affordable, and easy for everyone to use.

*A Simple Analogy:*

Imagine Cardano as a city with a main highway (Layer 1), where all the traffic (transactions) flows. Over time, as the city grows, the main highway can become congested, leading to slower travel times and higher costs for everyone.

Midgard adds a set of “express lanes” (Layer 2) running alongside the main highway. These extra lanes handle a large volume of traffic more efficiently, allowing more cars to move smoothly without overloading the main highway.

By doing so, Midgard helps ensure the Cardano network remains fast, affordable, and convenient for all its users.

#pagebreak()
\
= Understanding Layer 2 Solutions
\
== The Need for Layer 2
\
  As blockchain networks like Cardano become more popular, they face challenges in handling a high number of transactions efficently. 
  
  *High transaction volumes can lead to network congestion*, causing slower transaction times and higher fees. 
  
  *Layer 2 solutions* help relieve this pressure by providing *faster*, *cheaper*, and *more scalable* ways to process transactions.

\  
== How Layer 2 Works
\
  Layer 2 solutions run on top of the main blockchain *(Layer 1)* but execute transactions *off-chain*. 
  
  This means the base blockchain remains secure and decentralized, while the transaction processing happens separately. 
  
By moving most processing off the main chain, the system can handle more transactions at once and reduce overall costs.

\
== Types of Layer 2 Solutions
\
+ *Optimistic Rollups*

  - Assume transactions are valid by default.

  - Only verify transactions if a fraud proof is submitted.
  - Midgard L2 utilizes optimistic rollups to effectively scale the Cardano blockchain.
  
+ *State Channels*

  - Allow participants to conduct transactions off-chain.

  - Only the final state is recorded on the main blockchain.

+ *Sidechains*

  - Separate blockchains connected to the main chain.

  - Handle specific types of transactions independently.

+ *Plasma*

  - Creates smaller chains linked to the main blockchain.

  - Ensures scalability by offloading processing.

#pagebreak()
#v(50pt)

\
= Key Concepts
\

+ *Layer 2 (Extra Lanes for Traffic)*

  - *Midgard L2* operates on top of *Cardano’s* main blockchain *(Layer 1)* to help handle more transactions at once.

  - It uses a special technology called *optimistic rollups* to manage transactions efficiently.

+ *Operators*

  - Operators are like traffic controllers who manage the flow of transactions in the express lanes.
  
  - They handle transactions off the main highway and then periodically report back to the main blockchain with updates to keep traffic flowing smoothly. 
  
  - To ensure they do their job honestly, Operators must deposit *ADA* (Cardano’s native currency) as a bond. If they cheat, they lose this bond, ensuring they act in the system’s best interest.

+  *Watchers*

  - Watchers are like undercover inspectors who keep an eye on the Operators to ensure no one is cheating. 
  
  - If they spot any suspicious activity, they raise a red flag (submit a “*fraud  proof*”) to challenge the Operator’s actions.
  
  - If the fraud proof is valid, the bad update is canceled, and the Operator may face penalties.

+  *Fraud Proofs*

  - A fraud proof is basically evidence that something went wrong with the transactions.
  
  - During a designated challenge period, Watchers can submit fraud proofs to contest invalid state commitments.
  
  - If a fraud proof is validated, the fraudulent update is undone and penalizes the dishonest Operator and they may lose their bond.

+  *State Management – Keeping Track*

  - Midgard uses a smart system to keep track of all transactions and states (like the current traffic conditions).
  
  - Instead of storing every single transaction in detail, Midgard creates a secure and concise summary using *Merkle roots*. This ensures everything stays organized and secure without handling every tiny detail.

+  *Optimistic Rollup*

  - Optimistic rollups assume that all transactions are valid by default, speeding up the process.
  
  - Only if a Watcher raises a concern (a fraud proof) does the system take a closer look to verify the transaction. 
  
  - This approach significantly increases transaction throughput and efficiency..

+  *Tokenless Design*

  - Midgard uses *ADA*, Cardano’s native currency, for all transactions and incentives.

  - There’s no need for an additional token, simplifying the system and maintaining focus on ADA.

#pagebreak()
\
= How It Works
  \
  == Transaction Processing
  \
  - Transactions are first handled off the main blockchain (off-chain) by Operators.

  - Operators bundle these transactions and prepare a summary (similar to a progress report) to send back to the main blockchain.
  \
== State Commitments
  \
  - Operators regularly send summaries (state commitments) to Cardano’s main blockchain.

  - These summaries include the current state, new transactions, and what the state will be next, all represented in a compact form using Merkle roots.
  \
== Challenge Period
  \
  - After an update is submitted, there’s a waiting period called the challenge period.

  - During this time, Watchers can check for any mistakes or fraudulent activities in the update.
  \
== Dispute Resolution
  \
  - If a Watcher submits a fraud proof during the challenge period, the system verifies it.

  - If the fraud proof is correct, the bad update is undone, and the responsible Operator may be penalized.
  - If no one raises a concern, the update is finalized and becomes part of the main blockchain.

#pagebreak()

\
= Execution Framework
\
== Midgard's Optimistic Rollup Architecture
\
At the heart of Midgard’s functionality is its *optimistic rollup* architecture, designed to scale Cardano while maintaining its security. 

This system combines several elements:

  - *Validator Network:* Ensures transactions are processed correctly.

  - *Efficient State Management:* Uses Merkle roots to create compact and secure sum-
maries of all activities, making it easy to keep everything organized.
\
=== Operator Network
\
  - *Midgard implements an on-chain linked list of Operators* who manage the Layer 2 state and submit state transitions to Cardano’s Layer 1.

  - *Operators must deposit ADA into a bond contract* on Layer 1 to secure their role.
  - The system uses a *rotating schedule* for Operators, giving each an exclusive turn to process Layer 2 events and commit their block of events.
  - *Operators verify transactions*, *update the state*, *prepare state commitments*, and *submit* these updates to the Layer 1 State Validator smart contract.
  \
=== State Management
\
  - Midgard’s state management revolves around a *State Validator smart contract* on Cardano’s Layer 1, optimized for the eUTxO model.

  - The system maintains three key components:

    + *Current State:* Represents valid Layer 2 transactions and balances.

    + *Incoming Transactions:* Transactions waiting to be processed.
    + *Next State:* The outcome after processing incoming transactions.

  - These components are represented as *Merkle roots*, allowing for efficient state transitions and verification.
\
=== UTxO-Optimized Fraud Proofs 
\
Leverages Cardano’s UTxO (Unspent Transaction Output) model to efficiently detect
and prove any dishonest activities without needing to review every single transaction in detail. This makes fraud detection faster and more effective.

\
=== Balanced Economic Incentives
\
-  Operators earn rewards for processing transactions correctly, while Watchers earn re-wards for successfully identifying and reporting fraud.Encourages honest participation and deters malicious activities.

- These incentives encourage honest participation and deters malicious activities, creating a fair and balanced system where everyone benefits from maintaining the network’s integrity.

#pagebreak()
\
== Fraud Proof System
\
A foundational component of Midgard’s execution framework is the fraud proof system. This system ensures the integrity and correctness of transactions by allowing participants to challenge any invalid state updates. 

Here’s how it works:

\
=== Challenge Period
\
After a state update is submitted, there is a designated time called the challenge period. During this time, participants (Watchers) can review the update and submit fraud proofs if they find any issues.

=== Submitting Fraud Proofs
\
To submit a fraud proof, a challenger provides evidence that a specific transaction or state transition is fraudulent. This involves presenting the original transaction data and secure summaries called Merkle proofs.

\
=== Challenge-Response Mechanism
\
When a Watcher contests the validity of a state update, both the Watcher and the Oper-ator who published the disputed state must actively participate in proving or disproving the claim. If the Watcher’s fraud proof is correct, the fraudulent update is canceled, and the Operator faces penalties.


#pagebreak()
\
== Types of Fraud Proofs
\
To maintain the integrity of the system, Watchers can submit various types of fraud proofs. These proofs help ensure that all transactions are valid and that the system remains secure. Here are the different types of fraud proofs:

\
=== Value Not Preserved Proofs
\
Ensures that the total value of inputs and outputs, minus fees, remains consistent. Prevents situations where value is lost or incorrectly calculated during transactions.

\
=== Timestamp Misuse Proofs
\
Validates that transaction and block timestamps are within allowed ranges. Preventsmanipulation of transaction times to gain unfair advantages or disrupt the system.

\
=== Improper Incentives/Fees Proofs
\
Ensures that transaction fees are calculated correctly. Prevents Operators from charging incorrect fees, ensuring fairness and transparency.

\
=== Double-Spending Proofs
\
Detects when the same UTXO is used in multiple transactions illegitimately. Prevents the same funds from being spent more than once, maintaining the integrity of transactions.

\
=== Invalid Signature Proofs
\
Proves that a UTXO is included in a transaction without the necessary cryptographic signature. Ensures that all transactions are authorized and secure, preventing unauthorized access.

#pagebreak()
\
== Seamless Integration with Cardano
\
Midgard seamlessly integrates with Cardano’s Layer 1 (L1) blockchain, ensuring that all off-chain transactions are secure and verifiable. 

Here’s how it achieves this:

- *Smart Contracts:* Midgard uses smart contracts to manage state transitions, ensuring that every update is properly recorded and verified on the main blockchain.

- *Merkle Roots:* These compact representations are used for efficient proof submissions, making sure that off-chain transactions remain organized and secure.
- *Native  Design:* Midgard’s design is tailored specifically for Cardano, leveraging its unique features to optimize performance and security.

#pagebreak()
\
= Benefits of Midgard
\
== Scale Up
\
- Midgard significantly *increases Cardano’s transaction capacity*, enabling the network to handle a much larger number of transactions at once.

- This prevents slowdowns and high costs, making Cardano capable of supporting a wide range of decentralized applications and services without compromising performance.
\
== One Honest Actor
\
- Unlike many traditional consensus mechanisms that require multiple honest participants, Midgard requires only one honest, active participant at all times to maintain system integrity.

- This simplifies the security model and reduces the complexity of maintaining the network’s trustworthiness.
\
== High Security
\
- Midgard combines optimistic rollups, a specialized fraud proof mechanism, and economic incentives to ensure the system remains secure.

- The use of Cardano’s UTxO model allows for detailed validation, making it difficult for fraudulent activities to go unnoticed.

- Watchers and Operators work together to maintain the integrity of the protocol.
\
== Efficiency & Lower Transaction Costs
\
- By processing transactions off-chain, Midgard reduces the overall cost of transactions.

- The combination of optimistic rollups and efficient state management ensures that the system remains both fast and economical.
- This makes Cardano more accessible and cost-effective for users, encouraging broader adoption.
\
== Seamless User Experience
\
- Midgard’s complexity is abstracted away (hidden behind the scenes), allowing users to interact with Midgard-enabled decentralized applications (dApps) without needing to switch networks or perform manual bridging.

#pagebreak()
\
= Why Midgard Matters
\
Midgard L2 plays a crucial role in enhancing the capabilities of the Cardano blockchain.

Here’s why it matters:

- *Speed & Scale:* Allows Cardano to handle a significantly larger number of transactions quickly, supporting more users and applications without slowdowns.

- *Lower Costs:* Reduces transaction fees by handling many processes  off the main blockchain, making Cardano more affordable for everyone.
- *High Security:* Maintains strong security standards through a combination of technology and economic incentives, ensuring the system remains safe from fraud and errors.
- *User-Friendly:* Provides a seamless experience for users, eliminating the need for extrasteps or complex processes when using Cardano-based applications.

By doing so, Midgard empowers developers and users to expand the Cardano ecosystem, fostering more innovation and adoption without compromising the network’s integrity.

#pagebreak()
\
= Conclusion
\
As Cardano continues to grow, Midgard provides the extra lanes, guardrails, and incentives needed to ensure it remains a high-performance, secure, and user-friendly blockchain platform. Through its optimistic rollup architecture, UTxO-optimized fraud proofs, and balanced economic incentives, Midgard ensures that Cardano can scale effectively, welcoming more users and applications into its ecosystem while maintaining the core values of security, decentralization, and trustworthiness.




