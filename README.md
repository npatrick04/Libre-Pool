# Enable the SPI bus and configure the MCP3008 library
This will establish the MCP3008 on the SPI bus as IIO device 1 on the libre board.
```
sudo ldto merge spicc
sudo ldto merge spicc-mcp3008
```

# Install SBCL and quicklisp
sudo apt install sbcl

Download quicklisp and install

# Compile the libre-pool executable
Put this library in ~/common-lisp
cd ~/common-lisp/libre-pool
make

# Create the service
Copy the libre-pool.service file to ~/.config/systemd/user

Enable the service with
```
systemctl --user enable libre-pool.service
```

Start the service with
```
systemctl --user restart libre-pool.service
```
