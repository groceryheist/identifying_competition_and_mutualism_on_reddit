df_pred = stan_negbin_var_predict(fit, N)

x_dates = pd.date_range(posts.created_week.max(), periods=forecast_len,freq='W')
dates = pd.DataFrame({'created_week' : x_dates, 'x':np.arange(df_pred['x'].min(), df_pred['x'].max()+1)})
df_pred = df_pred.merge(dates, on='x')

subs = pd.DataFrame({'subreddit' : posts_mat.columns, 'variable':[f"y{i}" for i in range(1, K+1)]})
df_pred = df_pred.merge(subs, on='variable')

posts = posts.drop('index',axis=1)
posts = posts.rename(columns={'n_posts':'y'})
posts['y_upper'] = posts.y
posts['y_lower'] = posts.y
df_pred = df_pred.drop(['x','variable'],axis=1)

plot_df = pd.concat([posts,df_pred],axis=0)
plot_df = plot_df.sort_values(['created_week','subreddit'])
subs_to_plot = ['politics','ChapoTrapHouse','WayOfTheBern','SandersForPresident','BernieSanders','OurPresident']

#p = ggplot(plot_df.loc[plot_df.subreddit.isin(subs_to_plot)], aes(y='y',ymax='y_upper', ymin='y_lower',x='created_week',group='subreddit', color='subreddit'))
p = ggplot(plot_df.loc[plot_df.subreddit.isin(subs_to_plot)], aes(y='y',ymax='y_upper', ymin='y_lower',x='created_week',group='subreddit', color='subreddit'))
p = p + geom_line() + geom_ribbon(alpha=0.1) + scale_y_log10()
p.save('plots/bernie_subs_posts_per_week_forecast.pdf')

comm_matrix = fit.extract('beta')['beta']

comm_matrix_mean = comm_matrix.mean(axis=0)
comm_matrix_q_90 = np.quantile(comm_matrix,0.9,axis=0)
comm_matrix_q_10 = np.quantile(comm_matrix,0.1,axis=0)

# now for no. distinct posters
