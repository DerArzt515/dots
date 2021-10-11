
tmux_ln="/home/arzt/.tmux.conf"
nvim_ln="/home/arzt/.config/nvim"

rm $tmux_ln
ln -s /home/arzt/dots/.tmux.conf $tmux_ln
rm $nvim_ln
ln -s /home/arzt/dots/nvim $nvim_ln 
