home_dir="/Users/mattcavalier"

tmux_ln="${home_dir}/.tmux.conf"
nvim_ln="${home_dir}/.config/nvim"

rm $tmux_ln
ln -s ${home_dir}/dots/.tmux.conf $tmux_ln
rm $nvim_ln
ln -s ${home_dir}/dots/nvim $nvim_ln 
