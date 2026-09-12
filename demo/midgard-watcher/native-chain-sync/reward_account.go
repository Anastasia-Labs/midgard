package main

import (
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"net"
	"time"

	ouroboros "github.com/blinklabs-io/gouroboros"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/protocol/localstatequery"
)

type wireStakeCredential struct {
	Hash string `json:"hash"`
	Type string `json:"type"`
}

type rewardAccountEvent struct {
	Credential      wireStakeCredential `json:"credential"`
	DepositLovelace *string             `json:"depositLovelace"`
	Kind            string              `json:"kind"`
	Point           wireBlockPoint      `json:"point"`
	PoolIDHash      *string             `json:"poolIdHash"`
	Registered      bool                `json:"registered"`
	RewardsLovelace string              `json:"rewardsLovelace"`
	SchemaVersion   string              `json:"schemaVersion"`
	StartupDigest   string              `json:"startupDigest"`
}

// Registration is membership in the ledger's deposit map. A reward summary
// can omit registered credentials which have never delegated to a pool/DRep.
// All values below come from one acquired ledger snapshot.
func writeRewardAccount(config startupConfig, startupCanonical []byte, writer *canonicalWriter) error {
	deadline := time.Now().Add(time.Duration(config.Operation.TimeoutMs) * time.Millisecond)
	conn, err := net.DialTimeout("unix", config.SocketPath, min(10*time.Second, time.Until(deadline)))
	if err != nil {
		return err
	}
	defer conn.Close()
	if err := conn.SetDeadline(deadline); err != nil {
		return err
	}
	connection, err := ouroboros.New(
		ouroboros.WithConnection(&queryLimitedConn{Conn: conn, remaining: maxQueryIngressBytes}),
		ouroboros.WithNetworkMagic(config.NetworkMagic),
		ouroboros.WithNodeToNode(false),
		ouroboros.WithErrorChan(make(chan error, 4)),
		ouroboros.WithLocalStateQueryConfig(localstatequery.NewConfig()),
	)
	if err != nil {
		return err
	}
	defer connection.Close()
	client := connection.LocalStateQuery().Client
	if err := client.AcquireVolatileTip(); err != nil {
		return err
	}
	defer client.Release()
	point, err := client.GetChainPoint()
	if err != nil {
		return err
	}
	blockNo, err := client.GetChainBlockNo()
	if err != nil {
		return err
	}
	if len(point.Hash) != 32 || blockNo < 0 {
		return errors.New("reward-account query requires a non-origin ledger point")
	}
	bytes, err := hex.DecodeString(config.Operation.Credential.Hash)
	if err != nil {
		return err
	}
	credential := localstatequery.StakeCredential{Bytes: ledger.NewBlake2b224(bytes)}
	if config.Operation.Credential.Type == "Script" {
		credential.Tag = 1
	}
	credentials := []localstatequery.StakeCredential{credential}
	deposits, err := client.GetStakeDelegDeposits(credentials)
	if err != nil {
		return err
	}
	accounts, err := client.GetFilteredDelegationsAndRewardAccounts(credentials)
	if err != nil {
		return err
	}
	deposit, registered := (*deposits)[credential]
	var depositLovelace, poolID *string
	if registered {
		value := fmt.Sprint(deposit)
		depositLovelace = &value
	}
	if pool, ok := accounts.Delegations[credential]; ok {
		value := pool.String()
		poolID = &value
	}
	digest := sha256.Sum256(startupCanonical)
	return writer.write(rewardAccountEvent{
		Credential:      *config.Operation.Credential,
		DepositLovelace: depositLovelace,
		Kind:            "reward_account",
		Point:           wireBlockPoint{BlockHash: hex.EncodeToString(point.Hash), BlockNo: fmt.Sprint(blockNo), Slot: fmt.Sprint(point.Slot)},
		PoolIDHash:      poolID,
		Registered:      registered,
		RewardsLovelace: fmt.Sprint(accounts.Rewards[credential]),
		SchemaVersion:   schemaVersion,
		StartupDigest:   hex.EncodeToString(digest[:]),
	})
}
