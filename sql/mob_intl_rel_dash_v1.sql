--user-treatment mapping for a day. only takes the last treatment a user had in a context on that day
--add the country_code into the table 
create volatile table mob_abtest as(
  select log_date_key
   , countrycode as country_code
   , bcookie
   , testname 
   , context
   , treatment
  from(
    select log_date_key
     , countrycode
     , bcookie 
     , testname 
     , context
     , coalesce(treatment_sl, treatment_a) treatment 
     , row_number() over(partition by bcookie,context,countrycode order by max_eventtime desc) treatment_num_desc
    from(
      select f_date_to_int(a.eventdate) log_date_key
       , countrycode
       , a.bcookie
       , a.testname
       , lower(coalesce(a.context,'n/a')) context
       , a.treatment treatment_a 
       , sl.treatment treatment_sl
       , max(a.eventtime) max_eventtime
      from user_groupondw.m_raw_abtest a

      left join sandbox.bli_rel_treatment_labels sl
        on lower(a.context) = lower(sl.context) 
        and a.treatment = sl.hash_003

      where a.eventdate = f_int_to_date(${datereplace})
        and a.countrycode not in ('US','CA')
        and a.bcookie is not null
        and lower(a.context) in ('mobile_featured_intl','mobile_getaways_intl','mobile_nearby_intl','mobile_goods_intl','mobile_search_intl')

        group by 1,2,3,4,5,6,7
    ) a
  ) b

  where treatment_num_desc = 1
) with data primary index(log_date_key,country_code,bcookie,testname)
  on commit preserve rows;


--insert the mobile_intl_feynman context to give overall system-wise results
insert into mob_abtest 
select log_date_key
 , countrycode as country_code
 , bcookie
 , testname
 , context
 , treatment
from(
    select log_date_key
     , countrycode
     , bcookie
     , testname
     , context
     , treatment
     , row_number() over(partition by bcookie,context,countrycode order by max_eventtime desc) treatment_num_desc
    from(
      select f_date_to_int(a.eventdate) log_date_key
       , countrycode
       , a.bcookie
       , a.testname
       , 'mobile_intl_feynman' context
       , case when a.treatment in ('87af85b1d09c8c2a979558ff9f81d10c','357916f20caffc33fb450edb74f61fbb','a8d82a009a4c73d0bc41107434c72c00','ea09af1ca648079d1431242f64ccb066','c9379d26475efb34b7b5ed2b18c5487')
          then 'feynman_treatments'
         else 'other_treatments'
         end as treatment
       , max(a.eventtime) max_eventtime
      from user_groupondw.m_raw_abtest a

      where a.eventdate = f_int_to_date(${datereplace})
        and a.countrycode in ('UK','AT')
        and a.bcookie is not null
        and lower(a.context) in ('mobile_featured_intl','mobile_nearby_intl','mobile_goods_intl','mobile_search_intl')

      group by 1,2,3,4,5,6
    ) a
) b

where treatment_num_desc = 1
;

--stats collection
collect stats column(log_date_key, country_code, bcookie) on BLI.mob_abtest;
collect stats column(testname, context, treatment) on BLI.mob_abtest;

--find 'actual' tab visits
create volatile table BLI.mob_visit as(
    select f_date_to_int(log_date) log_date_key
     , country_code
     , platform
     , bcookie
     , user_permalink
     , max(logged_out) logged_out
     , max(case when tab = 'featured' 
            and imps >= 3  --at least 3 impressions
            and ((max_eventtime - min_eventtime)/1000 >= 10) --at least 10 seconds
            then 1 else 0 end) featured_tab_visit

     , max(case when (tab='shopping' or tab = 'goods') 
            and imps >= 3 
            and ((max_eventtime - min_eventtime)/1000 >= 10) 
            then 1 else 0 end) goods_tab_visit

     , max(case when tab = 'global_search' 
            and imps >= 3 
            and ((max_eventtime - min_eventtime)/1000 >= 10) 
            then 1 else 0 end) search_tab_visit

     , max(case when tab = 'nearby' 
            and imps >= 3 
            and ((max_eventtime - min_eventtime)/1000 >= 10) 
            then 1 else 0 end) nearby_tab_visit

     , max(case when (tab='travel' or tab = 'groupon-getaways' or tab = 'getaways')
            and imps >= 3 
            and ((max_eventtime - min_eventtime)/1000 >= 10) 
            then 1 else 0 end) getaways_tab_visit

     , max(case when 
          (tab is not null or tab not in('featured','goods','shopping','global_search','nearby','travel','getaways','groupon-getaways')) 
          and (imps >= 3)
          and  ((max_eventtime - min_eventtime)/1000 >= 10) 
          then 1 else 0 end) other_tab_visit

     , max(case when imps >=3
                and ((max_eventtime - min_eventtime)/1000>=10)
                then 1 else 0
           end) all_tab_visit

    from user_groupondw.fact_mob_tab_events_int

    where log_date_key = ${datereplace}
      and platform <> 'Touch'

    group by 1,2,3,4,5
) with data primary index(log_date_key, country_code, bcookie)
  on commit preserve rows;

collect stats column(log_date_key, country_code, bcookie) on BLI.mob_visit;

--map active tab users to treatments
create volatile table mob_users as(
  select v.log_date_key
  , ab.country_code
  , ab.testname
  , ab.context
  , ab.treatment
  , v.platform
  , v.bcookie
  , v.user_permalink
  , cast(null as char(1)) gender
  , count(*) over(partition by v.bcookie) bcookie_users
  -- , v.logged_out
from BLI.mob_visit v 

join BLI.mob_abtest ab 
  on v.log_date_key = ab.log_date_key 
  and v.country_code = ab.country_code
  and v.bcookie = ab.bcookie

where (ab.context = 'mobile_featured_intl' and v.featured_tab_visit = 1)
   or (ab.context = 'mobile_goods_intl' and v.goods_tab_visit = 1)
   or (ab.context = 'mobile_search_intl' and v.search_tab_visit = 1)
   or (ab.context = 'mobile_nearby_intl' and v.nearby_tab_visit = 1)
   or (ab.context = 'mobile_getaways_intl' and v.getaways_tab_visit = 1)
   or (ab.context = 'mobile_intl_feynman' and v.all_tab_visit = 1)

) with data primary index(log_date_key, country_code, testname, context, treatment, bcookie, user_permalink)
  on commit preserve rows;

drop table BLI.mob_visit;
drop table BLI.mob_abtest;

--create gender volatile table + update mob_users
--need to replace the table dim_user by gbl_dim_user
create volatile table BLI.user_gender as(
select permalink user_permalink
 , max(gender) gender 
from user_groupondw.dim_user

where country not in('US','CA')
  and permalink is not null
  and gender is not null

group by 1
) with data primary index(user_permalink)
  on commit preserve rows;

update BLI.mob_users
from BLI.user_gender g 
set gender = g.gender
where BLI.mob_users.user_permalink = g.user_permalink;


--engagement metric agg
create volatile table engagement_agg as(
select f_date_to_int(e.log_date) log_date_key
 , e.country_code
 , e.bcookie 
 , e.user_permalink
 -- , e.tab
 -- , null deal_position
 -- , null mst_category
 -- , null deal_demand_channel
 -- , null deal_supply_channel
 -- , null deal_age
 -- , null market_size
 -- , null asp
 -- , null redemption_location
 -- , max(e.logged_out) logged_out
 , sum(e.searches) searches 
 , sum(e.imps) impressions
 , sum(e.uniq_imps) uniq_imps
 , sum(e.dvs) dvs
 , sum(e.uniq_dvs) uniq_dvs
from user_groupondw.fact_mob_tab_events_int e

where e.log_date = f_int_to_date(${datereplace})

group by 1,2,3,4 --,5,6
) with data primary index(country_code, bcookie, user_permalink)
  on commit preserve rows;

collect stats column(bcookie) on BLI.engagement_agg;


--create order metric agg
create volatile table orders_agg as(
select b.log_date_key
 , b.country_code
 , b.bcookie
 , b.user_permalink
 -- , b.order_tab
 -- , b.deal_position
 -- , b.deal_demand_channel
 -- , b.deal_supply_channel
 -- , b.mst_category
 -- , b.deal_age
 -- , b.market_size
 -- , b.asp
 -- , b.redemption_location
 , b.gross_bookings
 , b.gross_revenue
 , b.units
 -- , b.net_operational_bookings
 -- , b.net_operational_revenue
 -- , b.refund_amount
 -- , b.resignation_amount
 -- , b.discount_amount
 -- , b.orders
from(
select f_date_to_int(mo.log_date) log_date_key 
 , mo.country_code
 , mo.bcookie
 , mo.user_permalink
 -- , coalesce(mo.order_tab,'unknown') order_tab
 -- , case 
 --   when mo.imp_pos = 1 then '1'
 --   when mo.imp_pos = 2 then '2'
 --   when mo.imp_pos = 3 then '3'
 --   when mo.imp_pos < 26 then '4-25'
 --   when mo.imp_pos < 50 then '26-50'
 --   when mo.imp_pos < 100 then '51-100'
 --   else '100+' end deal_position
 -- , cast(null as varchar(255)) mst_category
 -- , cast(null as varchar(255)) deal_demand_channel
 -- , cast(null as varchar(255)) deal_supply_channel
 -- , cast(null as varchar(255)) deal_age
 -- , cast(null as varchar(255)) market_size
 -- , cast(null as varchar(255)) asp
 -- , cast(null as varchar(255)) redemption_location
 , sum(mo.gross_bookings) gross_bookings
 , sum(mo.gross_revenue) gross_revenue
 , sum(mo.units) units
 -- , sum(0.0) net_operational_bookings
 -- , sum(0.0) net_operational_revenue
 -- , sum(0.0) refund_amount
 -- , sum(0.0) resignation_amount
 -- , sum(0.0) discount_amount
 -- , sum(0) orders
from user_groupondw.fact_mob_orders_int mo 

--join user_groupondw.agg_deal ad 
--  on mo.deal_key = ad.deal_key 

--join user_groupondw.fact_order_master o
--  on mo.order_id = o.order_id

--left join sandbox.js_market_size ms
--  on ad.division_key = ms.division_key

where mo.log_date = f_int_to_date(${datereplace})
  and mo.bcookie is not null 

group by 1,2,3,4 --,5,6
) b

) with data primary index(log_date_key, country_code, bcookie)
  on commit preserve rows;

collect stats column(log_date_key, country_code, bcookie) on BLI.orders_agg;


create volatile table BLI.metric_agg as(
select c.log_date_key
 , c.country_code
 , c.bcookie
 , c.user_permalink
 -- , c.tab 
 -- , c.deal_position
 -- , c.mst_category
 -- , c.deal_demand_channel
 -- , c.deal_supply_channel
 -- , c.deal_age
 -- , c.market_size 
 -- , c.asp
 -- , c.redemption_location
 , sum(searches) searches
 , sum(impressions) impressions
 , sum(uniq_imps) uniq_imps
 , sum(dvs) dvs
 , sum(uniq_dvs) uniq_dvs
 , sum(gross_bookings) gross_bookings
 , sum(gross_revenue) gross_revenue
 , sum(units) units
 -- , sum(net_operational_bookings) net_operational_bookings
 -- , sum(net_operational_revenue) net_operational_revenue
 -- , sum(refund_amount) refund_amount
 -- , sum(resignation_amount) resignation_amount
 -- , sum(discount_amount) discount_amount
 -- , sum(orders) orders
from(
select a.log_date_key
 , a.country_code
 , cast(a.bcookie as varchar(255)) bcookie
 , cast(a.user_permalink as varchar(255)) user_permalink
 -- , cast(a.tab  as varchar(255)) tab
 -- , cast(a.deal_position as varchar(255)) deal_position
 -- , cast(a.mst_category as varchar(255)) mst_category
 -- , cast(a.deal_demand_channel as varchar(255)) deal_demand_channel
 -- , cast(a.deal_supply_channel as varchar(255)) deal_supply_channel
 -- , cast(a.deal_age as varchar(255)) deal_age
 -- , cast(a.market_size as varchar(255)) market_size
 -- , cast(a.asp as varchar(255)) asp
 -- , cast(a.redemption_location as varchar(255)) redemption_location
 , a.searches 
 , a.impressions
 , a.uniq_imps
 , a.dvs
 , a.uniq_dvs
 , null gross_bookings
 , null gross_revenue
 , null units
 -- , null net_operational_bookings
 -- , null net_operational_revenue
 -- , null refund_amount
 -- , null resignation_amount
 -- , null discount_amount
 -- , null orders
from BLI.engagement_agg a 

UNION ALL

select b.log_date_key
 , b.country_code
 , cast(b.bcookie as varchar(255)) bcookie
 , cast(b.user_permalink as varchar(255)) user_permalink
 -- , cast(b.order_tab as varchar(255)) tab
 -- , cast(b.deal_position as varchar(255)) deal_position
 -- , cast(b.mst_category as varchar(255)) mst_category
 -- , cast(b.deal_demand_channel as varchar(255)) deal_demand_channel
 -- , cast(b.deal_supply_channel as varchar(255)) deal_supply_channel
 -- , cast(b.deal_age as varchar(255)) deal_age
 -- , cast(b.market_size as varchar(255)) market_size
 -- , cast(b.ASP as varchar(255)) asp
 -- , cast(b.redemption_location as varchar(255)) redemption_location 
 , null searches 
 , null impressions
 , null uniq_imps
 , null dvs
 , null uniq_dvs
 , b.gross_bookings
 , b.gross_revenue
 , b.units
 -- , b.net_operational_bookings
 -- , b.net_operational_revenue
 -- , b.refund_amount
 -- , b.resignation_amount
 -- , b.discount_amount
 -- , b.orders
from BLI.orders_agg b 
) c

group by 1,2,3,4 --,5,6
) with data primary index(log_date_key
 , country_code
 , bcookie
 , user_permalink
 -- , tab 
 -- , mst_category
 -- , deal_demand_channel
 -- , deal_age
 -- , market_size
 -- , ASP
 ) on commit preserve rows;

 drop table BLI.engagement_agg;
 drop table BLI.orders_agg;

-- clean this records about this date
delete from sandbox.bli_intl_rel_mob_dash_v1
  where log_date_key = ${datereplace};

-- create table sandbox.bli_intl_rel_mob_dash_v1 as(
insert into sandbox.bli_intl_rel_mob_dash_v1 
select a.log_date_key
 , f_int_to_date(a.log_date_key) log_date
 , a.platform
 , a.country_code
 , a.testname
 , a.context
 , a.treatment
 -- , a.gender
 -- , a.tab 
 -- , a.deal_position
 -- , a.mst_category
 -- , a.deal_demand_channel
 -- , a.deal_supply_channel
 -- , a.deal_age
 -- , a.market_size 
 -- , a.asp
 -- , a.redemption_location
 , sum(a.searches) searches
 , sum(a.impressions) impressions
 , sum(a.uniq_imps) uniq_imps
 , sum(a.dvs) dvs
 , sum(a.uniq_dvs) uniq_dvs
 , sum(a.gross_bookings) gross_bookings
 , sum(a.gross_revenue) gross_revenue 
 , sum(a.units) quantity 
 -- , sum(a.net_operational_bookings) net_operational_bookings
 -- , sum(a.net_operational_revenue) net_operational_revenue
 -- , sum(a.refund_amount) refund_amount
 -- , sum(a.resignation_amount) resignation_amount
 -- , sum(a.discount_amount) discount_amount
 -- , sum(a.orders) orders
 , sum(a.searches_sq) searches_sq
 , sum(a.impressions_sq) impressions_sq
 , sum(a.uniq_imps_sq) uniq_imps_sq
 , sum(a.dvs_sq) dvs_sq
 , sum(a.uniq_dvs_sq) uniq_dvs_sq
 , sum(a.gross_bookings_sq) gross_bookings_sq
 , sum(a.gross_revenue_sq) gross_revenue_sq
 , sum(a.units_sq) quantity_sq 
 -- , sum(a.net_operational_bookings_sq) net_operational_bookings_sq
 -- , sum(a.net_operational_revenue_sq) net_operational_revenue_sq
 -- , sum(a.refund_amount_sq) refund_amount_sq
 -- , sum(a.resignation_amount_sq) resignation_amount_sq
 -- , sum(a.discount_amount_sq) discount_amount_sq
 -- , sum(a.orders_sq) orders_sq
 , sum(a.user_count) user_count
from( 
select u.log_date_key
 , u.country_code
 , u.platform
 , u.testname
 , u.context
 , u.treatment
 -- , u.gender
 -- , m.tab 
 -- , m.deal_position
 -- , m.mst_category
 -- , m.deal_demand_channel
 -- , m.deal_supply_channel
 -- , m.deal_age
 -- , m.market_size 
 -- , m.asp
 -- , m.redemption_location
 , sum(m.searches) searches
 , sum(m.impressions) impressions
 , sum(m.uniq_imps) uniq_imps
 , sum(m.dvs) dvs
 , sum(m.uniq_dvs) uniq_dvs
 , sum(m.gross_bookings) gross_bookings
 , sum(m.gross_revenue) gross_revenue
 , sum(m.units) units
 -- , sum(m.net_operational_bookings) net_operational_bookings
 -- , sum(m.net_operational_revenue) net_operational_revenue
 -- , sum(m.refund_amount) refund_amount
 -- , sum(m.resignation_amount) resignation_amount
 -- , sum(m.discount_amount) discount_amount
 -- , sum(m.orders) orders
 , sum(m.searches**2) searches_sq
 , sum(m.impressions**2) impressions_sq
 , sum(m.uniq_imps**2) uniq_imps_sq
 , sum(m.dvs**2) dvs_sq
 , sum(m.uniq_dvs**2) uniq_dvs_sq
 , sum(m.gross_bookings**2) gross_bookings_sq
 , sum(m.gross_revenue**2) gross_revenue_sq
 , sum(m.units**2) units_sq
 -- , sum(m.net_operational_bookings**2) net_operational_bookings_sq
 -- , sum(m.net_operational_revenue**2) net_operational_revenue_sq
 -- , sum(m.refund_amount**2) refund_amount_sq
 -- , sum(m.resignation_amount**2) resignation_amount_sq
 -- , sum(m.discount_amount**2) discount_amount_sq
 -- , sum(m.orders**2) orders_sq
 , sum(0) user_count
from BLI.mob_users u

left join BLI.metric_agg m 
  on u.bcookie = m.bcookie
  and u.country_code = m.country_code
  and u.user_permalink = m.user_permalink

group by 1,2,3,4,5,6 --,7,8,9,10,11,12,13,14,15

UNION ALL

select b.log_date_key 
 , b.country_code
 , b.platform
 , b.testname
 , b.context 
 , b.treatment
 -- , b.gender
 -- , cast(null as varchar(255)) tab
 -- , cast(null as varchar(255)) deal_position
 -- , cast(null as varchar(255)) mst_category
 -- , cast(null as varchar(255)) deal_demand_channel
 -- , cast(null as varchar(255)) deal_supply_channel
 -- , cast(null as varchar(255)) deal_age
 -- , cast(null as varchar(255)) market_size
 -- , cast(null as varchar(255)) asp
 -- , cast(null as varchar(255)) redemption_location
 , null searches
 , null impressions
 , null uniq_imps
 , null dvs
 , null uniq_dvs
 , null gross_bookings
 , null gross_revenue
 , null units
 -- , null net_operational_bookings
 -- , null net_operational_revenue
 -- , null refund_amount
 -- , null resignation_amount
 -- , null discount_amount
 -- , null orders
 , null searches_sq
 , null impressions_sq
 , null uniq_imps_sq
 , null dvs_sq
 , null uniq_dvs_sq
 , null gross_bookings_sq
 , null gross_revenue_sq
 , null units_sq
 -- , null net_operational_bookings_sq
 -- , null net_operational_revenue_sq
 -- , null refund_amount_sq
 -- , null resignation_amount_sq
 -- , null discount_amount_sq
 -- , null orders_sq
 , b.user_count
from (
  select ub.log_date_key
   , ub.country_code
   , ub.platform
   , ub.testname
   , ub.context
   , ub.treatment
   -- , ub.gender
   , sum(case when user_permalink is null then
            case when bcookie_users = 1 then 1
            else 0 end 
          else 1 end) user_count
  from BLI.mob_users ub
  group by 1,2,3,4,5,6
) b
  --counts non-null bcookie/permalink combos and null permalinks only when the 
  --associated bcookie has no other non-null permalink on that day
) a

group by 1,2,3,4,5,6,7 --,8,9,10,11,12,13,14,15
; 
-- ) with data primary index(log_date_key
--  , country_code
--  , testname
--  , context
--  , treatment
-- -- , tab 
-- -- , mst_category
-- -- , deal_demand_channel
-- -- , deal_age
-- -- , market_size 
-- -- , asp
-- );

