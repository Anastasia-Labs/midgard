package config

import (
	"bytes"
	"os"
	"path/filepath"

	"github.com/Anastasia-Labs/midgard-node-go/pkg"
	"github.com/spf13/viper"
)

type NodeConfig struct {
	ProviderKey   string
	Network       string
	SeedPhrase    string
	CryptoKeyPath string
	Peers         []string
	LogLevel      string
}

func NewNodeConfig() (*NodeConfig, error) {
	pkg.LoadConfig()
	return &NodeConfig{
		ProviderKey:   viper.GetString("node.providerkey"),
		Network:       viper.GetString("node.network"),
		SeedPhrase:    viper.GetString("node.seedphrase"),
		CryptoKeyPath: viper.GetString("crypto.keypath"),
		Peers:         viper.GetStringSlice("node.peers"),
		LogLevel:      viper.GetString("logger.loglevel"),
	}, nil
}

func ReadConfig(vi *viper.Viper, files []string) error {
	for i, file := range files {
		if _, err := os.Stat(file); os.IsNotExist(err) {
			return err
		}

		value, err := os.ReadFile(file)
		if err != nil {
			return err
		}

		ext := filepath.Ext(file)[1:]
		vi.SetConfigType(ext)
		reader := bytes.NewBuffer(value)
		if i == 0 {
			err = vi.ReadConfig(reader)
		} else {
			err = vi.MergeConfig(reader)
		}
		if err != nil {
			return err
		}
	}

	return nil
}
