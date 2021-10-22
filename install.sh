home_dir="/home/arzt"

tmux_ln="${home_dir}/.tmux.conf"
nvim_ln="${home_dir}/.config/nvim"
emacs_ln="${home_dir}/.emacs.d"

rm $tmux_ln
ln -s ${home_dir}/dots/.tmux.conf $tmux_ln
rm $nvim_ln
ln -s ${home_dir}/dots/nvim $nvim_ln 
rm -rf $emacs_ln
mkdir ${home_dir}/.emacs.d
ln -s ${home_dir}/dots/init.el ${emacs_ln}/init.el 
