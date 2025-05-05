package entity

import (
	"context"
	"crypto/rand"
	"encoding/json"
	"fmt"
	"os"

	libp2p "github.com/libp2p/go-libp2p"
	pubsub "github.com/libp2p/go-libp2p-pubsub"
	"github.com/libp2p/go-libp2p/core/crypto"
	corehost "github.com/libp2p/go-libp2p/core/host"
	peer "github.com/libp2p/go-libp2p/core/peer"
	ma "github.com/multiformats/go-multiaddr"
	"go.uber.org/zap"
)

type Node struct {
	Host    corehost.Host
	PubSub  *pubsub.PubSub
	Topic   *pubsub.Topic
	Sub     *pubsub.Subscription
	Mempool *Mempool
	Ctx     context.Context
	logger  *zap.SugaredLogger
}

// loadOrCreatePrivateKey loads the private key from a file or generates a new one if it doesn't exist
func loadOrCreatePrivateKey(cryptoKeyPath string) (crypto.PrivKey, error) {
	if _, err := os.Stat(cryptoKeyPath); err == nil {
		keyBytes, err := os.ReadFile(cryptoKeyPath)
		if err != nil {
			return nil, fmt.Errorf("failed to read private key file: %w", err)
		}
		privKey, err := crypto.UnmarshalPrivateKey(keyBytes)
		if err != nil {
			return nil, fmt.Errorf("failed to unmarshal private key: %w", err)
		}
		return privKey, nil
	}

	privKey, _, err := crypto.GenerateKeyPairWithReader(crypto.Ed25519, 2048, rand.Reader)
	if err != nil {
		return nil, fmt.Errorf("failed to generate private key: %w", err)
	}

	keyBytes, err := crypto.MarshalPrivateKey(privKey)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal private key: %w", err)
	}
	if err := os.WriteFile(cryptoKeyPath, keyBytes, 0600); err != nil {
		return nil, fmt.Errorf("failed to write private key to file: %w", err)
	}

	return privKey, nil
}

// NewNode creates a libp2p host, connects to bootstrap peers, and subscribes to pubsub topic
func NewNode(ctx context.Context, mempool *Mempool, peers []string, cryptoKeyPath string, logger *zap.SugaredLogger) (*Node, error) {
	privKey, err := loadOrCreatePrivateKey(cryptoKeyPath)
	if err != nil {
		return nil, err
	}
	h, err := libp2p.New(libp2p.Identity(privKey))
	if err != nil {
		return nil, err
	}
	for _, addr := range peers {
		if addr == "" {
			continue
		}
		maddr, err := ma.NewMultiaddr(addr)
		if err != nil {
			logger.Debug("Invalid multiaddr:", err)
			continue
		}
		info, err := peer.AddrInfoFromP2pAddr(maddr)
		if err != nil {
			logger.Debug("Failed to parse peer addr:", err)
			continue
		}
		if err := h.Connect(ctx, *info); err != nil {
			logger.Debug("Connection failed:", err)
		} else {
			logger.Info("Connected to peer:", info.ID)
		}
	}
	ps, err := pubsub.NewGossipSub(ctx, h)
	if err != nil {
		return nil, err
	}
	topic, err := ps.Join("tx-topic")
	if err != nil {
		return nil, err
	}
	sub, err := topic.Subscribe()
	if err != nil {
		return nil, err
	}

	node := &Node{
		Host:    h,
		PubSub:  ps,
		Topic:   topic,
		Sub:     sub,
		Mempool: mempool,
		logger:  logger,
		Ctx:     ctx,
	}
	go node.listen()

	logger.Info("Node ID:", h.ID())
	for _, a := range h.Addrs() {
		logger.Info(fmt.Sprintf(" - %s/p2p/%s\n", a, h.ID().String()))
	}
	return node, nil
}

func (n *Node) listen() {
	for {
		msg, err := n.Sub.Next(n.Ctx)
		if err != nil {
			n.logger.Debug("Sub error:", err)
			continue
		}
		// Ignore messages sent by this node
		if msg.ReceivedFrom == n.Host.ID() {
			continue
		}
		var tx Transaction
		if err := json.Unmarshal(msg.Data, &tx); err != nil {
			continue
		}
		n.logger.Info("RX from peer:", tx.ID)
		n.Mempool.Add(tx)
	}
}

func (n *Node) Broadcast(tx Transaction) error {
	data, err := json.Marshal(tx)
	if err != nil {
		return err
	}
	return n.Topic.Publish(n.Ctx, data)
}
